{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Byron.Config
    ( -- * Integration Launcher
      withCardanoNode
    ) where

import Prelude

import Cardano.BM.Trace
    ( Trace )
import Cardano.Chain.Genesis
    ( GenesisData (..), readGenesisData )
import Cardano.Crypto.ProtocolMagic
    ( ProtocolMagicId (..) )
import Cardano.Launcher
    ( Command (..), StdStream (..), withBackendProcess )
import Cardano.Wallet.Byron.Compatibility
    ( NodeVersionData, fromGenesisData )
import Cardano.Wallet.Byron.Network
    ( AddrInfo, localSocketAddrInfo )
import Cardano.Wallet.Byron.Transaction
    ( fromGenesisTxOut )
import Cardano.Wallet.Logging
    ( trMessageText )
import Cardano.Wallet.Network.Ports
    ( getRandomPort )
import Cardano.Wallet.Primitive.AddressDerivation
    ( hex )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , BlockHeader (..)
    , BlockchainParameters (..)
    , Hash (..)
    , SlotId (..)
    , TxOut (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.Exception
    ( bracket, throwIO )
import Control.Monad
    ( forM_, unless, when )
import Control.Monad.Fail
    ( MonadFail )
import Data.Aeson
    ( toJSON )
import Data.Coerce
    ( coerce )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Time.Clock.POSIX
    ( getPOSIXTime )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (..), nodeToClientCodecCBORTerm )
import System.Directory
    ( copyFile, doesDirectoryExist, removeDirectoryRecursive )
import System.Environment
    ( lookupEnv )
import System.FilePath
    ( (</>) )
import System.IO.Temp
    ( createTempDirectory, getCanonicalTemporaryDirectory )
import Test.Utils.Paths
    ( getTestData )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Yaml

data CardanoNodeConfig = CardanoNodeConfig
    { nodeGenesisHash  :: Text
    , nodeConfigFile   :: FilePath
    , nodeDatabaseDir  :: FilePath
    , nodeDlgCertFile  :: FilePath
    , nodeGenesisFile  :: FilePath
    , nodeSignKeyFile  :: FilePath
    , nodeSocketFile   :: FilePath
    , nodeTopologyFile :: FilePath
    }

-- | Spins up a @cardano-node@ in another process.
--
-- IMPORTANT: @cardano-node@ must be available on the current path.
withCardanoNode
    :: Trace IO Text
    -- ^ Some trace for logging
    -> (AddrInfo -> Block -> (BlockchainParameters, NodeVersionData) -> IO a)
    -- ^ Callback function with a socket description and genesis params
    -> IO a
withCardanoNode tr action =
    orThrow $ withConfig $ \cfg block0 (bp, vData) -> do
        nodePort <- getRandomPort
        let args = mkArgs cfg nodePort
        let cmd = Command "cardano-node" args (pure ()) Inherit Inherit
        withBackendProcess (trMessageText tr) cmd $ do
            let addrInfo = localSocketAddrInfo (nodeSocketFile cfg)
            action addrInfo block0 (bp, vData)
  where
    orThrow = (=<<) (either throwIO pure)
    mkArgs cfg port =
        [ "run"
        , "--config", nodeConfigFile cfg
        , "--signing-key", nodeSignKeyFile cfg
        , "--delegation-certificate", nodeDlgCertFile cfg
        , "--topology", nodeTopologyFile cfg
        , "--database-path", nodeDatabaseDir cfg
        , "--socket-path", nodeSocketFile cfg
        , "--genesis-file", nodeGenesisFile cfg
        , "--genesis-hash", T.unpack (nodeGenesisHash cfg)
        , "--port", show port
        ]


-- | Generate a new integration configuration based on a partial configuration
-- located in @./test/data/cardano-node@.
--
-- The 'startTime' from the partial genesis file will be overriden with a new
-- fresh recent one (resulting in a different genesis hash).
--
-- As a result, this function creates a temporary directory which is cleaned up
-- after use (unless the ENV var NO_CLEANUP is set):
--
--     $ tree /tmp/cardano-wallet-byron-2cbb1ea94edb1cea/
--       ├── genesis.json
--       ├── node.cert
--       ├── node.config
--       ├── node.key
--       └── node.topology
--
withConfig
    :: (  CardanoNodeConfig
       -> Block
       -> (BlockchainParameters, NodeVersionData)
       -> IO a
       )
    -- ^ Callback function with the node configuration and genesis params
    -> IO a
withConfig action =
    bracket setupConfig teardownConfig $ \(_a,b,c,d) -> action b c d
  where
    source :: FilePath
    source = $(getTestData) </> "cardano-node"

    setupConfig
        :: IO ( FilePath
              , CardanoNodeConfig
              , Block
              , (BlockchainParameters, NodeVersionData)
              )
    setupConfig = do
        dir <- getCanonicalTemporaryDirectory
            >>= \tmpRoot -> createTempDirectory tmpRoot "cardano-wallet-byron"

        let nodeConfigFile   = dir </> "node.config"
        let nodeDatabaseDir  = dir </> "node.db"
        let nodeDlgCertFile  = dir </> "node.cert"
        let nodeGenesisFile  = dir </> "genesis.json"
        let nodeSignKeyFile  = dir </> "node.key"
        let nodeSocketFile   = dir </> "node.socket"
        let nodeTopologyFile = dir </> "node.topology"

        Yaml.decodeFileThrow @_ @Aeson.Value (source </> "genesis.yaml")
            >>= withObject updateStartTime
            >>= Aeson.encodeFile nodeGenesisFile
        forM_ ["node.config", "node.topology", "node.key", "node.cert"] $ \f ->
            copyFile (source </> f) (dir </> f)

        (genesisData, genesisHash) <- unsafeRunExceptT $
            readGenesisData nodeGenesisFile
        let (bp, outs) = fromGenesisData (genesisData, genesisHash)

        let networkMagic = NetworkMagic $ unProtocolMagicId $ gdProtocolMagicId genesisData
        let nodeGenesisHash = T.decodeUtf8 $ hex $ getHash $ getGenesisBlockHash bp

        pure
            ( dir
            , CardanoNodeConfig
                { nodeGenesisHash
                , nodeConfigFile
                , nodeDatabaseDir
                , nodeDlgCertFile
                , nodeGenesisFile
                , nodeSignKeyFile
                , nodeSocketFile
                , nodeTopologyFile
                }
            , mkBlock0 bp outs
            , ( bp
              , ( NodeToClientVersionData { networkMagic }
                , nodeToClientCodecCBORTerm
                )
              )
            )

    teardownConfig
        :: (FilePath, b, c, d)
        -> IO ()
    teardownConfig (dir, _, _, _) = do
        noCleanup <- maybe False (not . null) <$> lookupEnv "NO_CLEANUP"
        exists <- doesDirectoryExist dir
        unless noCleanup $ when exists $ removeDirectoryRecursive dir

-- | Add a "startTime" field in a given object with the current POSIX time as a
-- value.
updateStartTime
    :: Aeson.Object
    -> IO Aeson.Object
updateStartTime m = do
    time <- round @_ @Int <$> getPOSIXTime
    pure $ HM.insert "startTime" (toJSON time) m

-- | Do something with an a JSON object. Fails if the given JSON value isn't an
-- object.
withObject
    :: MonadFail m
    => (Aeson.Object -> m Aeson.Object)
    -> Aeson.Value
    -> m Aeson.Value
withObject action = \case
    Aeson.Object m -> Aeson.Object <$> action m
    _ -> fail
        "withObject: was given an invalid JSON. Expected an Object but got \
        \something else."

-- | Construct an initial genesis block from a genesis UTxO.
mkBlock0 :: BlockchainParameters -> [TxOut] -> Block
mkBlock0 bp outs = Block
    { delegations  = []
    , header = BlockHeader
        { slotId =
            SlotId 0 0
        , blockHeight =
            Quantity 0
        , headerHash =
            coerce $ getGenesisBlockHash bp
        , parentHeaderHash =
            Hash (BS.replicate 32 0)
        }
    , transactions = fromGenesisTxOut <$> outs
    }

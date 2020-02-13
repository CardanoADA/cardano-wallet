{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDerivation.ShelleySpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( AccountingStyle (..)
    , ChainCode (..)
    , Depth (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index
    , MkKeyFingerprint (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PaymentAddress (..)
    , SoftDerivation (..)
    , SomeMnemonic
    , WalletKey (..)
    , XPrv
    , XPub (..)
    , paymentAddress
    )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( KnownNetwork (..)
    , ShelleyKey (..)
    , generateKeyFromSeed
    , minSeedLengthBytes
    , publicKeySize
    , unsafeGenerateKeyFromSeed
    )
import Cardano.Wallet.Primitive.AddressDerivationSpec
    ()
import Cardano.Wallet.Primitive.Types
    ( Address (..) )
import Control.Exception
    ( SomeException, evaluate )
import Data.Either
    ( isLeft, isRight )
import Data.List
    ( isSubsequenceOf )
import Test.Hspec
    ( Spec, describe, it, shouldThrow )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , arbitraryBoundedEnum
    , choose
    , elements
    , expectFailure
    , property
    , suchThat
    , vectorOf
    , (===)
    , (==>)
    )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

spec :: Spec
spec = do
    describe "Bounded / Enum relationship" $ do
        it "Calling toEnum for invalid value gives a runtime err (AccountingStyle)"
            (property prop_toEnumAccountingStyle)

    describe "Enum Roundtrip" $ do
        it "AccountingStyle" (property prop_roundtripEnumAccountingStyle)

    describe "BIP-0044 Derivation Properties" $ do
        it "deriveAccountPrivateKey works for various indexes" $
            property prop_accountKeyDerivation
        it "N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i)" $
            property prop_publicChildKeyDerivation

    describe "Encoding" $ do
        let cc = ChainCode "<ChainCode is not used by singleAddressToKey>"

        let userException str (e :: SomeException) = str `isSubsequenceOf` show e

        it "throws when encoding XPub of invalid length (Mainnet)" $ do
            let msg = "length was 2, but expected to be 33"
            evaluate (paymentAddress @'Mainnet (ShelleyKey $ XPub "\148" cc))
                `shouldThrow` userException msg

        it "throws when encoding XPub of invalid length (Testnet)" $ do
            let msg = "length was 2, but expected to be 33"
            evaluate (paymentAddress @'Testnet (ShelleyKey $ XPub "\148" cc))
                `shouldThrow` userException msg

    describe "KeyFingerprint" $ do
        it "Single addresses have a payment key but no delegation key"
            (property prop_fingerprintSingleAddress)
        it "Grouped addresses have a payment key and a delegation key"
            (property prop_fingerprintGroupedAddress)
        it "Inspecting Invalid addresses throws"
            (property prop_fingerprintInvalidAddress)
        it "Roundtrips paymentKeyFingerprint . liftPaymentFingerprint (Testnet)"
            (property (prop_fingerprintRoundtrip @'Testnet))
        it "Roundtrips paymentKeyFingerprint . liftPaymentFingerprint (Mainnet)"
            (property (prop_fingerprintRoundtrip @'Mainnet))

{-------------------------------------------------------------------------------
                               Properties
-------------------------------------------------------------------------------}

prop_toEnumAccountingStyle :: Int -> Property
prop_toEnumAccountingStyle n =
    n > fromEnum UTxOInternal ==> expectFailure $ property $
        (toEnum n :: AccountingStyle) `seq` ()

prop_roundtripEnumAccountingStyle :: AccountingStyle -> Property
prop_roundtripEnumAccountingStyle ix =
    (toEnum . fromEnum) ix === ix

-- | Deriving address public key should be equal to deriving address
-- private key and extracting public key from it (works only for non-hardened
-- child keys).
--
-- To compute the public child key of a parent private key:
--  * N(CKDpriv((kpar, cpar), i)) (works always).
--  * CKDpub(N(kpar, cpar), i) (works only for non-hardened child keys).
--
-- Thus:
--
-- N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i)
--
-- if (kpar, cpar) is a non-hardened key.
--
-- For details see <https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki#private-parent-key--public-child-key bip-0039>
prop_publicChildKeyDerivation
    :: (SomeMnemonic, Maybe SomeMnemonic)
    -> Passphrase "encryption"
    -> AccountingStyle
    -> Index 'Soft 'AddressK
    -> Property
prop_publicChildKeyDerivation (seed, recPwd) encPwd cc ix =
    addrXPub1 === addrXPub2
  where
    accXPrv = unsafeGenerateKeyFromSeed (seed, recPwd) encPwd :: ShelleyKey 'AccountK XPrv
    -- N(CKDpriv((kpar, cpar), i))
    addrXPub1 = publicKey $ deriveAddressPrivateKey encPwd accXPrv cc ix
    -- CKDpub(N(kpar, cpar), i)
    addrXPub2 = deriveAddressPublicKey (publicKey accXPrv) cc ix

prop_accountKeyDerivation
    :: (SomeMnemonic, Maybe SomeMnemonic)
    -> Passphrase "encryption"
    -> Index 'Hardened 'AccountK
    -> Property
prop_accountKeyDerivation (seed, recPwd) encPwd ix =
    accXPub `seq` property () -- NOTE Making sure this doesn't throw
  where
    rootXPrv = generateKeyFromSeed (seed, recPwd) encPwd :: ShelleyKey 'RootK XPrv
    accXPub = deriveAccountPrivateKey encPwd rootXPrv ix

-- | Single addresses have a payment key but no delegation key
prop_fingerprintSingleAddress
    :: SingleAddress 'Testnet
    -> Property
prop_fingerprintSingleAddress (SingleAddress addr) = property $
    isRight (paymentKeyFingerprint @ShelleyKey addr)

-- | Grouped addresses have a payment key and a delegation key
prop_fingerprintGroupedAddress
    :: GroupedAddress 'Testnet
    -> Property
prop_fingerprintGroupedAddress (GroupedAddress addr) = property $
    isRight (paymentKeyFingerprint @ShelleyKey addr)

-- | Inspecting Invalid addresses throws
prop_fingerprintInvalidAddress
    :: InvalidAddress
    -> Property
prop_fingerprintInvalidAddress (InvalidAddress addr) = property $
    isLeft (paymentKeyFingerprint @ShelleyKey addr)

-- | liftPaymentKeyFingerprint <$> paymentKeyFingerprint addr = Right addr
prop_fingerprintRoundtrip
    :: forall (n :: NetworkDiscriminant).
        ( PaymentAddress n ShelleyKey
        )
    => SingleAddress n
    -> Property
prop_fingerprintRoundtrip (SingleAddress addr) =
    (liftPaymentAddress @n <$> paymentKeyFingerprint @ShelleyKey addr)
        === Right addr

{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary AccountingStyle where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance {-# OVERLAPS #-} Arbitrary (Passphrase "seed") where
    arbitrary = do
        n <- choose (minSeedLengthBytes, 64)
        bytes <- BS.pack <$> vectorOf n arbitrary
        return $ Passphrase $ BA.convert bytes

newtype SingleAddress (n :: NetworkDiscriminant)
    = SingleAddress Address
    deriving (Eq, Show)

instance KnownNetwork n => Arbitrary (SingleAddress n) where
    arbitrary = SingleAddress . Address . BS.pack
        <$> fmap ([addrSingle @n] <>) (vectorOf publicKeySize arbitrary)

newtype GroupedAddress (n :: NetworkDiscriminant)
    = GroupedAddress Address
    deriving (Eq, Show)

instance KnownNetwork n => Arbitrary (GroupedAddress n) where
    arbitrary = GroupedAddress . Address . BS.pack
        <$> fmap ([addrGrouped @n] <>) (vectorOf (2*publicKeySize) arbitrary)

newtype InvalidAddress = InvalidAddress Address deriving (Eq, Show)

instance Arbitrary InvalidAddress where
    arbitrary = InvalidAddress . Address . BS.pack <$> do
        firstByte <- suchThat arbitrary (`notElem` validFirstBytes)
        len <- elements validSizes
        ([firstByte]<>) <$> vectorOf len arbitrary
      where
        validSizes = [publicKeySize, 2*publicKeySize]
        validFirstBytes =
            [ addrSingle @'Testnet
            , addrSingle @'Mainnet
            , addrGrouped @'Testnet
            , addrGrouped @'Mainnet
            ]

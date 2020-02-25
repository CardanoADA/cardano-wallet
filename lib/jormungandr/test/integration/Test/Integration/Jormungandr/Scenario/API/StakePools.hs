{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Jormungandr.Scenario.API.StakePools
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiFee
    , ApiNetworkInformation (..)
    , ApiNetworkParameters (..)
    , ApiStakePool
    , ApiT (..)
    , ApiTransaction
    , ApiWallet
    , BackwardCompatPlaceholder (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , Direction (..)
    , EpochLength (..)
    , EpochNo (..)
    , PoolId (..)
    , SlotLength (..)
    , SlotParameters (..)
    , TxStatus (..)
    )
import Control.Monad
    ( forM_ )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.List
    ( find )
import Data.Maybe
    ( isJust, isNothing )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( fromText, toText )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe, it, shouldBe, shouldSatisfy )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , TxDescription (..)
    , delegating
    , delegationFee
    , emptyRandomWallet
    , emptyWallet
    , eventually
    , eventuallyUsingDelay
    , expectErrorMessage
    , expectField
    , expectListField
    , expectListSize
    , expectResponseCode
    , faucetUtxoAmt
    , fixturePassphrase
    , fixtureRandomWallet
    , fixtureWallet
    , fixtureWalletWith
    , getFromResponse
    , joinStakePool
    , json
    , mkEpochInfo
    , notDelegating
    , quitStakePool
    , request
    , unsafeRequest
    , verify
    , waitForNextEpoch
    , walletId
    , withMethod
    , (.>)
    , (.>=)
    )
import Test.Integration.Framework.TestData
    ( errMsg403DelegationFee
    , errMsg403NotDelegating
    , errMsg403NotDelegatingNext
    , errMsg403PoolAlreadyJoined
    , errMsg403WrongPass
    , errMsg404NoEndpoint
    , errMsg404NoSuchPool
    , errMsg404NoWallet
    , errMsg405
    , errMsg406
    , errMsg415
    , falseWalletIds
    , getHeaderCases
    )
import Test.Integration.Jormungandr.Fixture
    ( OwnerIdentity (..), registerStakePool )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall t n. (n ~ 'Testnet) => SpecWith (Context t)
spec = do
    it "STAKE_POOLS_LIST_01 - List stake pools" $ \ctx -> do
        eventually "Listing stake pools shows expected information" $ do
            r <- request @[ApiStakePool] ctx Link.listStakePools Default Empty
            expectResponseCode HTTP.status200 r
            -- With the current genesis.yaml we have 3 pools with 1 lovelace,
            -- and an epoch length of 3.
            --
            -- For some reason, the first pool (the node we run), produces
            -- blocks in 100% of the /slots/. This means it will have produced
            -- either 1 or 2 blocks in the current epoch.
            verify r
                [ expectListSize 3

                , expectListField 0
                    #metadata ((`shouldBe` Just "Genesis Pool") . fmap (view #name))
                , expectListField 1
                    #metadata ((`shouldBe` Just "Genesis Pool") . fmap (view #name))
                , expectListField 2
                    #metadata ((`shouldBe` Just "Genesis Pool") . fmap (view #name))

                , expectListField 0
                    #cost (`shouldBe` (Quantity 0))
                , expectListField 1
                    #cost (`shouldBe` (Quantity 0))
                , expectListField 2
                    #cost (`shouldBe` (Quantity 0))

                , expectListField 0
                    #margin (`shouldBe` (Quantity minBound))
                , expectListField 1
                    #margin (`shouldBe` (Quantity minBound))
                , expectListField 2
                    #margin (`shouldBe` (Quantity minBound))

                , expectListField 0
                    (#metrics . #controlledStake) (`shouldBe` Quantity 1000)
                , expectListField 1
                    (#metrics . #controlledStake) (`shouldBe` Quantity 1000)
                , expectListField 2
                    (#metrics . #controlledStake) (`shouldBe` Quantity 1000)

                , expectListField 0
                    (#metrics . #producedBlocks) (.> Quantity 1)
                , expectListField 1
                    (#metrics . #producedBlocks) (`shouldBe` Quantity 0)
                , expectListField 2
                    (#metrics . #producedBlocks) (`shouldBe` Quantity 0)

                , expectListField 0
                    #apparentPerformance (.> 0)
                , expectListField 1
                    #apparentPerformance (`shouldBe` 0)
                , expectListField 2
                    #apparentPerformance (`shouldBe` 0)

                , expectListField 0
                    #desirability (.>= 0)
                , expectListField 1
                    #desirability (.>= 0)
                , expectListField 2
                    #desirability (.>= 0)

                , expectListField 0
                    #saturation (.>= 0)
                , expectListField 1
                    #saturation (.>= 0)
                , expectListField 2
                    #saturation (.>= 0)
                ]

    it "STAKE_POOLS_LIST_02 - May fail on epoch boundaries" $ \ctx -> do
        -- We should be able to catch the stake-pool data in an un-synced state
        -- when we enter into a new epoch. The results should then be
        -- unavailible.
        --
        -- This might take a few tries (epoch changes), so it is only feasible
        -- to test with very short epochs.
        let ms = 1000
        eventuallyUsingDelay (50*ms)
            "Shows error when listing stake pools on epoch boundaries"
            $ do
            r <- request @[ApiStakePool] ctx Link.listStakePools Default Empty
            verify r
                [ expectResponseCode HTTP.status503
                , expectErrorMessage
                    "I can't list stake pools yet because I need to scan the \
                    \blockchain for metrics first. I'm at"
                ]

    describe "STAKE_POOLS_LIST_03 - v2/stake-pools - Methods Not Allowed" $ do
        let methods = ["POST", "PUT", "DELETE", "CONNECT", "TRACE", "OPTIONS"]
        forM_ methods $ \method -> it (show method) $ \ctx -> do
            r <- request @ApiStakePool ctx
                    (method, "v2/stake-pools") Default Empty
            expectResponseCode @IO HTTP.status405 r
            expectErrorMessage errMsg405 r

    it "STAKE_POOLS_LIST_04 - Discovers new pools when they are registered" $ \ctx -> do
        let nWithMetadata = length . filter (isJust . view #metadata)
        let nWithoutMetadata = length . filter (isNothing . view #metadata)

        (_, pools) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty

        (poolIdA, poolAOwner)  <- registerStakePool ctx WithMetadata
        (poolIdB, _poolBOwner) <- registerStakePool ctx WithoutMetadata
        (poolIdC, poolCOwner)  <- registerStakePool ctx WithMetadata

        waitForNextEpoch ctx
        (_, pools') <- eventually "Stake pools are listed again" $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty

        nWithoutMetadata pools' `shouldBe` nWithoutMetadata pools + 1
        nWithMetadata pools' `shouldBe` nWithMetadata pools + 2

        let (Just poolA) = find ((== ApiT poolIdA) . view #id) pools'
        fmap (view #owner) (poolA ^. #metadata) `shouldBe` Just poolAOwner

        let (Just poolB) = find ((== ApiT poolIdB) . view #id) pools'
        (poolB ^. #metadata) `shouldBe` Nothing

        let (Just poolC) = find ((== ApiT poolIdC) . view #id) pools'
        fmap (view #owner) (poolC ^. #metadata) `shouldBe` Just poolCOwner

    it "STAKE_POOLS_JOIN_01 - Can join a stakepool" $ \ctx -> do
        w <- fixtureWallet ctx
        (_, p:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty

        -- Join a pool
        joinStakePool ctx (p ^. #id) (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]

        -- Wait for the certificate to be inserted
        eventually "Certificates are insterted" $ do
            let ep = Link.listTransactions @'Shelley w
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListField 0
                    (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 0
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]

    it "STAKE_POOLS_JOIN_01 - Controlled stake increases when joining" $ \ctx -> do
        w <- fixtureWallet ctx
        (_, Right (p:_)) <- eventually "Stake pools are listed" $
            request @[ApiStakePool] ctx Link.listStakePools Default Empty

        -- Join a pool
        joinStakePool ctx (p ^. #id) (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]

        -- Wait for the certificate to be inserted
        eventually "Certificates are insterted" $ do
            let ep = Link.listTransactions @'Shelley w
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListField 0
                    (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 0
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]

        let (fee, _) = ctx ^. #_feeEstimator $ DelegDescription 1 1 1
        let existingPoolStake = getQuantity $ p ^. #metrics . #controlledStake
        let contributedStake = faucetUtxoAmt - fee
        eventually "Controlled stake increases for the stake pool" $ do
            v <- request @[ApiStakePool] ctx Link.listStakePools Default Empty
            verify v
                [ expectListField 0 (#metrics . #controlledStake)
                    (.> Quantity (existingPoolStake + contributedStake))
                    -- No exact equality since the delegation from previous
                    -- tests may take effect.
                ]

    it "STAKE_POOLS_JOIN_04 - Rewards accumulate and stop" $ \ctx -> do
        w <- fixtureWallet ctx
        (_, p:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty

        -- Join a pool
        joinStakePool ctx (p ^. #id) (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]
        eventually "Certificates are inserted" $ do
            let ep = Link.listTransactions @'Shelley w
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListField 0
                    (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 0
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]

        -- Wait for money to flow
        eventually "Wallet gets rewards" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectField (#balance . #getApiT . #reward)
                    (.> (Quantity 0))
                ]

        -- Quit a pool
        quitStakePool ctx placeholder (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]
        eventually "Certificates are inserted after quiting a pool" $ do
            let ep = Link.listTransactions @'Shelley w
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListField 0
                    (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 0
                    (#status . #getApiT) (`shouldBe` InLedger)
                , expectListField 1
                    (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 1
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]

        waitForNextEpoch ctx
        waitForNextEpoch ctx
        reward <- getFromResponse (#balance . #getApiT . #reward) <$>
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty

        waitForNextEpoch ctx
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField
                    (#balance . #getApiT . #reward)
                    (`shouldBe` reward)
            ]

    it "STAKE_POOLS_JOIN_04 -\
        \Delegate, stop in the next epoch, and still earn rewards" $ \ctx -> do
        w <- fixtureWallet ctx
        (_, p1:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty

        joinStakePool ctx (p1 ^. #id) (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            ]

        waitForNextEpoch ctx

        quitStakePool ctx placeholder (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            ]

        reward <- eventually "wallet gets rewards" $ do
            r <- request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty
            verify r
                [ expectField
                        (#balance . #getApiT . #reward)
                        (.> Quantity 0)
                ]
            pure $ getFromResponse (#balance . #getApiT . #reward) r

        waitForNextEpoch ctx

        eventually "rewards are the same in the next epoch" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectField
                        (#balance . #getApiT . #reward)
                        (`shouldBe` reward)
                ]

    describe "STAKE_POOLS_JOIN_01x - Fee boundary values" $ do
        it "STAKE_POOLS_JOIN_01x - \
            \I can join if I have just the right amount" $ \(ctx) -> do
            (_, p:_) <- eventually "Stake pools are listed" $
                unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
            let (fee, _) = ctx ^. #_feeEstimator $ DelegDescription 1 0 1
            w <- fixtureWalletWith ctx [fee]
            joinStakePool ctx (p ^. #id) (w, "Secure Passphrase")>>= flip verify
                [ expectResponseCode HTTP.status202
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                ]

        it "STAKE_POOLS_JOIN_01x - \
            \I cannot join if I have not enough fee to cover" $ \ctx -> do
            (_, p:_) <- eventually "Stake pools are listed" $
                unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
            let (fee, _) = ctx ^. #_feeEstimator $ DelegDescription 1 0 1
            w <- fixtureWalletWith ctx [fee - 1]
            r <- joinStakePool ctx (p ^. #id) (w, "Secure Passphrase")
            expectResponseCode HTTP.status403 r
            expectErrorMessage (errMsg403DelegationFee 1) r

        it "STAKE_POOLS_JOIN_01x - I cannot join stake-pool with 0 balance" $ \ctx -> do
            (_, p:_) <- eventually "Stake pools are listed" $
                unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
            w <- emptyWallet ctx
            let (fee, _) = ctx ^. #_feeEstimator $ DelegDescription 0 0 1
            r <- joinStakePool ctx (p ^. #id) (w, "Secure Passphrase")
            expectResponseCode HTTP.status403 r
            expectErrorMessage (errMsg403DelegationFee fee) r

    describe "STAKE_POOLS_QUIT_01x - Fee boundary values" $ do
        it "STAKE_POOLS_QUIT_01x - \
            \I can quit if I have enough to cover fee" $ \(ctx) -> do
            let (feeJoin, _) = ctx ^. #_feeEstimator $ DelegDescription 1 1 1
            let (feeQuit, _) = ctx ^. #_feeEstimator $ DelegDescription 1 0 1
            let initBalance = [feeJoin + feeQuit]
            (w, _) <- joinStakePoolWithWalletBalance ctx initBalance
            rq <- quitStakePool ctx placeholder (w, "Secure Passphrase")
            expectResponseCode HTTP.status202 rq
            eventually "Wallet is not delegating and has balance = 0" $ do
                request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                    [ expectField #delegation (`shouldBe` notDelegating [])
                    -- balance is 0 because the rest was used for fees
                    , expectField
                        (#balance . #getApiT . #total) (`shouldBe` Quantity 0)
                    , expectField
                        (#balance . #getApiT . #available) (`shouldBe` Quantity 0)
                    ]

        it "STAKE_POOLS_QUIT_01x - \
            \I cannot quit if I have not enough fee to cover" $ \ctx -> do
            let (feeJoin, _) = ctx ^. #_feeEstimator $ DelegDescription 1 1 1
            let (feeQuit, _) = ctx ^. #_feeEstimator $ DelegDescription 0 0 1
            let initBalance = [feeJoin+1]
            (w, _) <- joinStakePoolWithWalletBalance ctx initBalance
            rq <- quitStakePool ctx placeholder (w, "Secure Passphrase")
            verify rq
                [ expectResponseCode HTTP.status403
                , expectErrorMessage (errMsg403DelegationFee (feeQuit - 1))
                ]

    it "STAKE_POOLS_QUIT_01x - \
        \Backward compatibility with pool ids" $ \ctx -> do
        w <- fixtureWallet ctx
        (_, p1:p2:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty

        -- Join a pool
        joinStakePool ctx (p1 ^. #id) (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]
        eventually "Last outgoing tx is in ledger" $ do
            let ep = Link.listTransactions @'Shelley w
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListField 0
                    (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 0
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]

        -- Quit delegation with an arbitrary id
        -- NOTE: We use another id purposely, just to show that the id has no
        -- influence but doesn't lead to an API error.
        let pid = BackwardCompat (Identity (p2 ^. #id))
        quitStakePool ctx pid (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]

    it "STAKE_POOLS_JOIN_01 - Cannot join non-existant stakepool" $ \ctx -> do
        let poolIdAbsent = PoolId $ BS.pack $ replicate 32 0
        w <- emptyWallet ctx
        r <- joinStakePool ctx (ApiT poolIdAbsent) (w, "Secure Passphrase")
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoSuchPool (toText poolIdAbsent)) r

    it "STAKE_POOLS_JOIN_01 - \
        \ If a wallet joins a stake pool, others are not affected" $ \ctx -> do
        (wA, wB) <- (,) <$> fixtureWallet ctx <*> fixtureWallet ctx
        (_, p:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty

        -- Join a pool
        joinStakePool ctx (p ^. #id) (wA, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]

        -- Wait for the certificate to be inserted
        eventually "Last outgoing tx is in ledger" $ do
            let ep = Link.listTransactions @'Shelley wA
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListField 0
                    (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 0
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]

        -- Verify the wallet is now delegating
        (currentEpochNo, sp) <- getSlotParams ctx
        request @ApiWallet ctx (Link.getWallet @'Shelley wA) Default Empty >>= flip verify
            [ expectField #delegation
                (`shouldBe` notDelegating
                    [ (Just (p ^. #id), mkEpochInfo (currentEpochNo + 2) sp)
                    ]
                )
            ]

        -- Verify the other is NOT delegating
        request @ApiWallet ctx (Link.getWallet @'Shelley wB) Default Empty >>= flip verify
            [ expectField #delegation (`shouldBe` notDelegating [])
            ]

    it "STAKE_POOLS_JOIN_02 - Passphrase must be correct to join" $ \ctx -> do
        (_, p:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
        w <- fixtureWallet ctx
        r <- joinStakePool ctx (p ^. #id) (w, "Incorrect Passphrase")
        expectResponseCode HTTP.status403 r
        expectErrorMessage errMsg403WrongPass r

    describe "STAKE_POOLS_JOIN/QUIT_02 -\
        \ Passphrase must have appropriate length" $ do

        let pMax = passphraseMaxLength (Proxy @"encryption")
        let pMin = passphraseMinLength (Proxy @"encryption")
        let tooShort =
                "passphrase is too short: expected at least 10 characters"
        let tooLong =
                "passphrase is too long: expected at most 255 characters"
        let tests =
                [ (tooLong, replicate (pMax + 1) '1')
                , (tooShort, replicate (pMin - 1) '1')
                ]
        let verifyIt ctx doStakePool pass expec = do
                (_, p:_) <- eventually "Stake pools are listed" $ do
                    unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
                w <- emptyWallet ctx
                r <- doStakePool ctx (p ^. #id) (w, T.pack pass)
                expectResponseCode HTTP.status400 r
                expectErrorMessage expec r

        forM_ tests $ \(expec, passphrase) -> do
            it ("Join: " ++ expec) $ \ctx -> do
                verifyIt ctx joinStakePool passphrase expec

            it ("Quit: " ++ expec) $ \ctx -> do
                verifyIt ctx (\_ _ -> quitStakePool ctx placeholder) passphrase expec

    describe "STAKE_POOLS_JOIN/QUIT_02 - Passphrase must be text" $ do
        let verifyIt ctx sPoolEndp = do
                (_, p:_) <- eventually "Stake pools are listed" $
                    unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
                w <- emptyWallet ctx
                let payload = Json [json| {
                        "passphrase": 123
                        } |]
                r <- request @(ApiTransaction n) ctx (sPoolEndp p w)
                        Default payload
                expectResponseCode HTTP.status400 r
                expectErrorMessage "expected Text, encountered Number" r
        it "Join" $ \ctx -> do
            verifyIt ctx Link.joinStakePool
        it "Quit" $ \ctx -> do
            verifyIt ctx (\_ -> Link.quitStakePool placeholder)

    it "STAKE_POOLS_JOIN_03 - Byron wallet cannot join stake pool" $ \ctx -> do
        (_, p:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
        w <- emptyRandomWallet ctx
        r <- joinStakePool ctx (p ^. #id) (w, "Secure Passprase")
        expectResponseCode HTTP.status404 r

    -- NOTE
    -- This is only true because:
    --
    -- 1/ We are in Jörmungandr scenario were fees can be known exactly
    -- 2/ Fixture wallets are made of homogeneous UTxOs (all equal to the same
    -- value) and therefore, the random selection has no influence.
    it "STAKE_POOLS_ESTIMATE_FEE_01 - fee matches eventual cost" $ \ctx -> do
        (_, p:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
        w <- fixtureWallet ctx
        r <- delegationFee ctx w
        print r
        verify r
            [ expectResponseCode HTTP.status200
            ]
        let fee = getFromResponse #amount r
        joinStakePool ctx (p ^. #id) (w, fixturePassphrase) >>= flip verify
            [ expectField #amount (`shouldBe` fee)
            ]

    it "STAKE_POOLS_ESTIMATE_FEE_01x - edge-case fee in-between coeff" $ \ctx -> do
        let (feeMin, _) = ctx ^. #_feeEstimator $ DelegDescription 1 0 1
        w <- fixtureWalletWith ctx [feeMin + 1, feeMin + 1]
        r <- delegationFee ctx w
        let (fee, _) = ctx ^. #_feeEstimator $ DelegDescription 2 1 1
        verify r
            [ expectResponseCode HTTP.status200
            , expectField #amount (`shouldBe` Quantity fee)
            ]

    it "STAKE_POOLS_ESTIMATE_FEE_02 - \
        \empty wallet cannot estimate fee" $ \ctx -> do
        w <- emptyWallet ctx
        let (fee, _) = ctx ^. #_feeEstimator $ DelegDescription 0 0 1
        delegationFee ctx w >>= flip verify
            [ expectResponseCode HTTP.status403
            , expectErrorMessage $ errMsg403DelegationFee fee
            ]

    it "STAKE_POOLS_ESTIMATE_FEE_03 - can't use byron wallets" $ \ctx -> do
        w <- fixtureRandomWallet ctx
        let ep = Link.getDelegationFee w
        r <- request @(ApiTransaction 'Mainnet) ctx ep Default Empty
        verify r
            [ expectResponseCode HTTP.status404 -- should fail
            , expectErrorMessage $ errMsg404NoWallet (w ^. walletId)
            ]

    describe "STAKE_POOLS_ESTIMATE_FEE_04 - false wallet ids" $ do
        forM_ falseWalletIds $ \(wDesc, walId) -> do
            let path = "wallets/" <> walId
            it ("wallet:" ++ wDesc) $ \ctx -> do
                let endpoint = "v2/" <> T.pack path <> "/delegations/fees"
                rg <- request @ApiFee ctx ("GET", endpoint) Default Empty
                expectResponseCode @IO HTTP.status404 rg
                if wDesc == "40 chars hex"
                then expectErrorMessage (errMsg404NoWallet $ T.pack walId) rg
                else expectErrorMessage errMsg404NoEndpoint rg

    describe "STAKE_POOLS_ESTIMATE_FEE_05 -\
        \ v2/wallets/{wid}/delegations/fees - Methods Not Allowed" $ do
        let methods = ["POST", "PUT", "DELETE", "CONNECT", "TRACE", "OPTIONS"]
        forM_ methods $ \method -> it (show method) $ \ctx -> do
            w <- emptyWallet ctx
            let ep = "v2/wallets/"<> w ^. walletId <> "/delegations/fees"
            r <- request @ApiFee ctx (method, ep) Default Empty
            expectResponseCode @IO HTTP.status405 r
            expectErrorMessage errMsg405 r

    describe "STAKE_POOLS_ESTIMATE_FEE_06 - HTTP headers" $ do
        forM_ (getHeaderCases HTTP.status200)
            $ \(title, headers, exps) -> it title $ \ctx -> do
                w <- fixtureWallet ctx
                let ep = Link.getDelegationFee w
                r <- request @ApiFee ctx ep headers Empty
                verify r exps

    describe "STAKE_POOLS_JOIN/QUIT_05 - Bad request" $ do
        let verifyIt ctx sPoolEndp = do
                w <- emptyWallet ctx
                let payload = NonJson  "{ passphrase: Secure Passphrase }"
                r <- request @(ApiTransaction n) ctx
                    (sPoolEndp (Identity arbitraryPoolId) w) Default payload
                expectResponseCode HTTP.status400 r

        it "Join" $ \ctx -> do
            verifyIt ctx Link.joinStakePool
        it "Quit" $ \ctx -> do
            verifyIt ctx (\_ -> Link.quitStakePool placeholder)

    describe "STAKE_POOLS_JOIN/QUIT_05 -  Methods Not Allowed" $ do
        let methods = ["POST", "CONNECT", "TRACE", "OPTIONS"]
        forM_ methods $ \method -> it ("Join: " ++ show method) $ \ctx -> do
            w <- emptyWallet ctx
            let payload = Json [json| {
                    "passphrase": "Secure Passphrase"
                    } |]
            let link = withMethod method $
                    Link.joinStakePool (Identity arbitraryPoolId) w
            r <- request @(ApiTransaction n) ctx link Default payload
            expectResponseCode HTTP.status405 r
            expectErrorMessage errMsg405 r

    describe "STAKE_POOLS_JOIN/QUIT_05 - HTTP headers" $ do
        let verifyIt ctx sPoolEndp headers expec = do
                w <- emptyWallet ctx
                let payload = Json [json| {
                        "passphrase": "Secure Passphrase"
                        } |]
                r <- request @(ApiTransaction n) ctx
                        (sPoolEndp (Identity arbitraryPoolId) w) headers payload
                verify r expec

        let payloadHeaderCases =
                    [ ( "No HTTP headers -> 415", None
                       , [ expectResponseCode @IO HTTP.status415
                         , expectErrorMessage errMsg415 ]
                       )
                     , ( "Accept: text/plain -> 406"
                       , Headers
                             [ ("Content-Type", "application/json")
                             , ("Accept", "text/plain") ]
                       , [ expectResponseCode @IO HTTP.status406
                         , expectErrorMessage errMsg406 ]
                       )
                     , ( "No Content-Type -> 415"
                       , Headers [ ("Accept", "application/json") ]
                       , [ expectResponseCode @IO HTTP.status415
                         , expectErrorMessage errMsg415 ]
                       )
                     , ( "Content-Type: text/plain -> 415"
                       , Headers [ ("Content-Type", "text/plain") ]
                       , [ expectResponseCode @IO HTTP.status415
                         , expectErrorMessage errMsg415 ]
                       )
                     ]
        forM_ payloadHeaderCases $ \(title, headers, expectations) -> do
            it ("Join: " ++ title) $ \ctx ->
                verifyIt ctx Link.joinStakePool headers expectations
        forM_ payloadHeaderCases $ \(title, headers, expectations) -> do
            it ("Quit: " ++ title) $ \ctx ->
                verifyIt ctx (\_ -> Link.quitStakePool placeholder) headers expectations

    it "STAKE_POOLS_QUIT_02 - Passphrase must be correct to quit" $ \ctx -> do
        (w, _) <- joinStakePoolWithFixtureWallet ctx

        r <- quitStakePool ctx placeholder (w, "Incorrect Passphrase")
        expectResponseCode HTTP.status403 r
        expectErrorMessage errMsg403WrongPass r

    it "STAKE_POOL_NEXT_01 - Can join/re-join another/quit stake pool" $ \ctx -> do
        (_, p1:p2:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
        w <- fixtureWallet ctx

        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation (`shouldBe` notDelegating [])
            ]

        -- make sure we are at the beginning of new epoch
        (currentEpoch, sp) <- getSlotParams ctx
        waitForNextEpoch ctx

        -- joining first pool
        r1 <- joinStakePool ctx (p1 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r1
        waitAllTxsInLedger ctx w
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation
                (`shouldBe` notDelegating
                    [ (Just (p1 ^. #id), mkEpochInfo (currentEpoch + 3) sp)
                    ]
                )
            ]
        eventually "Wallet is delegating to p1" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectField #delegation (`shouldBe` delegating (p1 ^. #id) [])
                ]

        -- rejoining second pool
        r2 <- joinStakePool ctx (p2 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r2
        waitAllTxsInLedger ctx w
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation
                (`shouldBe` delegating (p1 ^. #id)
                    [ (Just (p2 ^. #id), mkEpochInfo (currentEpoch + 5) sp)
                    ]
                )
            ]
        eventually "Wallet is delegating to p2" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectField #delegation (`shouldBe` delegating (p2 ^. #id) [])
                ]

        --quiting
        r3 <- quitStakePool ctx placeholder (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r3
        waitAllTxsInLedger ctx w
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation
                (`shouldBe` delegating (p2 ^. #id)
                    [ (Nothing, mkEpochInfo (currentEpoch + 7) sp)
                    ]
                )
            ]
        eventually "Wallet is not delegating" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectField #delegation (`shouldBe` notDelegating [])
                ]

    it "STAKE_POOL_NEXT_02 - Override join with join in the same epoch =>\
        \ delegating to the last one in the end" $ \ctx -> do
        (_, p1:p2:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
        w <- fixtureWallet ctx

        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty
            >>= flip verify
                [ expectField #delegation (`shouldBe` notDelegating []) ]

        -- make sure we are at the beginning of new epoch
        (currentEpoch, sp) <- getSlotParams ctx
        waitForNextEpoch ctx

        let joinPool pool = do
                r1 <- joinStakePool ctx (pool ^. #id) (w, fixturePassphrase)
                expectResponseCode HTTP.status202 r1
                waitAllTxsInLedger ctx w
                request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                    [ expectField #delegation
                        (`shouldBe` notDelegating
                            [ (Just (pool ^. #id), mkEpochInfo (currentEpoch + 3) sp)
                            ]
                        )
                    ]

        -- join p1 and p2 in the same epoch and make sure delegation next is
        -- always to the last one joined
        joinPool p1
        joinPool p2

    it "STAKE_POOL_NEXT_02 - Override join with quit in the same epoch =>\
        \ not_delegating in the end" $ \ctx -> do
        (_, pool:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
        w <- fixtureWallet ctx

        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty
            >>= flip verify
                [ expectField #delegation (`shouldBe` notDelegating []) ]

        -- make sure we are at the beginning of new epoch
        (currentEpoch, sp) <- getSlotParams ctx
        waitForNextEpoch ctx

        -- join SP
        r1 <- joinStakePool ctx (pool ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r1
        waitAllTxsInLedger ctx w
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation
                (`shouldBe` notDelegating
                    [ (Just (pool ^. #id), mkEpochInfo (currentEpoch + 3) sp)
                    ]
                )
            ]

        -- quit SP in the same epoch
        r2 <- quitStakePool ctx placeholder (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r2
        waitAllTxsInLedger ctx w
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation
                (`shouldBe` notDelegating
                    [ (Nothing, mkEpochInfo (currentEpoch + 3) sp)
                    ]
                )
            ]

    it "STAKE_POOL_NEXT_02 - Override quit with join in the same epoch =>\
        \ delegating to the last one in the end" $ \ctx -> do
        (_, pool:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
        w <- fixtureWallet ctx

        -- joined p1 => active: p1
        r1 <- joinStakePool ctx (pool ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r1
        eventually "Delegating to pool" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectField #delegation (`shouldBe` delegating (pool ^. #id) [])
                ]

        (currentEpoch, sp) <- getSlotParams ctx

        -- quit SP => active: pool, next: not_delegating
        r2 <- quitStakePool ctx placeholder (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r2
        waitAllTxsInLedger ctx w
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation
                (`shouldBe` delegating (pool ^. #id)
                    [ (Nothing, mkEpochInfo (currentEpoch + 2) sp)
                    ]
                )
            ]

        -- join SP in the same epoch => active: pool, next: pool
        r3 <- joinStakePool ctx (pool ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r3
        waitAllTxsInLedger ctx w
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation
                (`shouldBe` delegating (pool ^. #id)
                    [ (Just (pool ^. #id), mkEpochInfo (currentEpoch + 2) sp)
                    ]
                )
            ]

        -- Join again => you cannot!
        r <- joinStakePool ctx (pool ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status403 r
        let poolId = toText $ getApiT $ pool ^. #id
        expectErrorMessage (errMsg403PoolAlreadyJoined poolId) r

    it "STAKE_POOL_NEXT_03\
        \ Join and quit in two subsequent epochs => delegating in epoch X + 2,\
        \ not_delegating in epoch X + 3"
        $ \ctx -> do
        (_, p1:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
        w <- fixtureWallet ctx

        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation (`shouldBe` notDelegating []) ]

        -- make sure we are at the beginning of new epoch
        (currentEpoch, sp) <- getSlotParams ctx
        waitForNextEpoch ctx

        r1 <- joinStakePool ctx (p1 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r1
        waitAllTxsInLedger ctx w
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation
                (`shouldBe` notDelegating
                    [ (Just (p1 ^. #id), mkEpochInfo (currentEpoch + 3) sp)
                    ]
                )
            ]

        waitForNextEpoch ctx

        r2 <- quitStakePool ctx placeholder (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r2
        waitAllTxsInLedger ctx w
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation
                (`shouldBe` notDelegating
                    [ (Just (p1 ^. #id), mkEpochInfo (currentEpoch + 3) sp)
                    , (Nothing, mkEpochInfo (currentEpoch + 4) sp)
                    ]
                )
            ]

        -- coming to the next epoch:
        eventually "active: delegating to p1, next: not_delegating" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectField (#tip . #epochNumber . #getApiT)
                    (`shouldBe` currentEpoch + 3)
                , expectField #delegation
                    (`shouldBe` delegating (p1 ^. #id)
                        [ (Nothing, mkEpochInfo (currentEpoch + 4) sp) ]
                    )
                ]

    it "STAKE_POOL_NEXT_03\
        \ - Join 2 in two subsequent epochs => delegating to 1st in epoch X + 2\
        \ and 2nd in epoch X + 3"
        $ \ctx -> do
        (_, p1:p2:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
        w <- fixtureWallet ctx

        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation (`shouldBe` notDelegating []) ]

        -- make sure we are at the beginning of new epoch
        (currentEpoch, sp) <- getSlotParams ctx
        waitForNextEpoch ctx

        r1 <- joinStakePool ctx (p1 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r1
        waitAllTxsInLedger ctx w
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation
                (`shouldBe` notDelegating
                    [ (Just (p1 ^. #id), mkEpochInfo (currentEpoch + 3) sp)
                    ]
                )
            ]

        -- coming to the next epoch and should be not delegating with two nexts
        -- after another joining
        waitForNextEpoch ctx

        r2 <- joinStakePool ctx (p2 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r2
        waitAllTxsInLedger ctx w
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation
                (`shouldBe` notDelegating
                    [ (Just (p1 ^. #id), mkEpochInfo (currentEpoch + 3) sp)
                    , (Just (p2 ^. #id), mkEpochInfo (currentEpoch + 4) sp)
                    ]
                )
            ]

        -- coming to the next epoch:
        --  active: delegating to p1, next: delegating to p2
        eventually "Delegating to p1 and about to delegate to p2" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectField (#tip . #epochNumber . #getApiT)
                    (`shouldBe` currentEpoch + 3)
                , expectField #delegation
                    (`shouldBe` delegating (p1 ^. #id)
                        [ (Just (p2 ^. #id), mkEpochInfo (currentEpoch + 4) sp) ]
                    )
                ]

    it "STAKE_POOL_NEXT_04/STAKE_POOLS_QUIT_01 - Cannot quit when active: not_delegating"
        $ \ctx -> do
        w <- emptyWallet ctx
        r <- quitStakePool ctx placeholder (w, "Secure Passprase")
        expectResponseCode HTTP.status403 r
        expectErrorMessage errMsg403NotDelegating r

    it "STAKE_POOL_NEXT_04\
        \ - Cannot quit when most recent delegation next: not_delegating"
        $ \ctx -> do
        (_, p1:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
        w <- fixtureWallet ctx

        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation (`shouldBe` notDelegating []) ]

        -- join p1
        r1 <- joinStakePool ctx (p1 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r1
        eventually "Delegating to p1" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectField #delegation (`shouldBe` delegating (p1 ^. #id) [])
                ]

        (currentEpoch, sp) <- getSlotParams ctx
        -- quit sp
        r2 <- quitStakePool ctx placeholder (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r2
        waitAllTxsInLedger ctx w
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation
                (`shouldBe` delegating (p1 ^. #id)
                    [ (Nothing, mkEpochInfo (currentEpoch + 2) sp)
                    ]
                )
            ]
        -- cannot quit again because you're already quitting
        r3 <- quitStakePool ctx placeholder (w, fixturePassphrase)
        expectResponseCode HTTP.status403 r3
        expectErrorMessage errMsg403NotDelegatingNext r3

    it "STAKE_POOL_NEXT_04 - Cannot quit when 'next 1': not_delegating,\
        \ 'next 2': not_delegating" $ \ctx -> do
        (_, p1:p2:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
        w <- fixtureWallet ctx

        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation (`shouldBe` notDelegating []) ]

        -- join p1
        r1 <- joinStakePool ctx (p1 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r1
        eventually "Delegating to p1" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectField #delegation (`shouldBe` delegating (p1 ^. #id) [])
                ]

        (currentEpoch, sp) <- getSlotParams ctx

        -- quit sp
        r2 <- quitStakePool ctx placeholder (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r2
        waitAllTxsInLedger ctx w
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation
                (`shouldBe` delegating (p1 ^. #id)
                    [ (Nothing, mkEpochInfo (currentEpoch + 2) sp)
                    ]
                )
            ]

        waitForNextEpoch ctx

        -- join p2
        r3 <- joinStakePool ctx (p2 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r3
        waitAllTxsInLedger ctx w
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation
                (`shouldBe` delegating (p1 ^. #id)
                    [ (Nothing, mkEpochInfo (currentEpoch + 2) sp)
                    , (Just (p2 ^. #id), mkEpochInfo (currentEpoch + 3) sp)
                    ]
                )
            ]

        -- can quit replacing next 2: p2 with next 2: not_delegating
        r4 <- quitStakePool ctx placeholder (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r4
        waitAllTxsInLedger ctx w
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation
                (`shouldBe` delegating (p1 ^. #id)
                    [ (Nothing, mkEpochInfo (currentEpoch + 2) sp)
                    , (Nothing, mkEpochInfo (currentEpoch + 3) sp)
                    ]
                )
            ]

        -- cannot quit again because you're already quitting
        r5 <- quitStakePool ctx placeholder (w, fixturePassphrase)
        expectResponseCode HTTP.status403 r5
        expectErrorMessage errMsg403NotDelegatingNext r5

    it "STAKE_POOL_NEXT_05/STAKE_POOLS_JOIN_01 - Cannot join A, when active: A"
        $ \ctx -> do
        w <- fixtureWallet ctx
        (_, p:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
        -- join p1
        r1 <- joinStakePool ctx (p ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r1
        eventually "Delegating to p" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectField #delegation (`shouldBe` delegating (p ^. #id) [])
                ]

        -- Join again
        r <- joinStakePool ctx (p ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status403 r
        let poolId = toText $ getApiT $ p ^. #id
        expectErrorMessage (errMsg403PoolAlreadyJoined poolId) r

    it "STAKE_POOL_NEXT_05/STAKE_POOLS_JOIN_01 - Cannot join A, when 'next 1': A\
        \ and there is no 'next 2'" $ \ctx -> do
        (_, p1:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
        w <- fixtureWallet ctx

        -- make sure we are at the beginning of new epoch
        (currentEpoch, sp) <- getSlotParams ctx
        waitForNextEpoch ctx

        r1 <- joinStakePool ctx (p1 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r1
        waitAllTxsInLedger ctx w
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation
                (`shouldBe` notDelegating
                    [ (Just (p1 ^. #id), mkEpochInfo (currentEpoch + 3) sp)
                    ]
                )
            ]

        -- Join again
        r <- joinStakePool ctx (p1 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status403 r
        let poolId = toText $ getApiT $ p1 ^. #id
        expectErrorMessage (errMsg403PoolAlreadyJoined poolId) r

    it "STAKE_POOL_NEXT_05/STAKE_POOLS_JOIN_01 - Cannot join A, when 'active' : A,\
        \ 'next 1': B and 'next 2': A" $ \ctx -> do
        (_, p1:p2:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
        w <- fixtureWallet ctx

        -- joined p1 => active: p1
        r1 <- joinStakePool ctx (p1 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r1
        eventually "Delegating to p1" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectField #delegation (`shouldBe` delegating (p1 ^. #id) [])
                ]

        (currentEpoch, sp) <- getSlotParams ctx

        -- join p2 => active: p1, next 1: p2
        r2 <- joinStakePool ctx (p2 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r2
        waitAllTxsInLedger ctx w
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation
                (`shouldBe` delegating (p1 ^. #id)
                    [ (Just (p2 ^. #id), mkEpochInfo (currentEpoch + 2) sp)
                    ]
                )
            ]

        waitForNextEpoch ctx

        -- Join p1 => active: p1, next 1: p2, next 2: p1
        r3 <- joinStakePool ctx (p1 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r3
        waitAllTxsInLedger ctx w
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation
                (`shouldBe` delegating (p1 ^. #id)
                    [ (Just (p2 ^. #id), mkEpochInfo (currentEpoch + 2) sp)
                    , (Just (p1 ^. #id), mkEpochInfo (currentEpoch + 3) sp)
                    ]
                )
            ]

        -- Join p1 => you cannot
        r4 <- joinStakePool ctx (p1 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status403 r4
        let poolId = toText $ getApiT $ p1 ^. #id
        expectErrorMessage (errMsg403PoolAlreadyJoined poolId) r4

waitAllTxsInLedger
    :: forall t n. (n ~ 'Testnet)
    => Context t
    -> ApiWallet
    -> IO ()
waitAllTxsInLedger ctx w = eventually "waitAllTxsInLedger: all txs in ledger" $ do
    let ep = Link.listTransactions @'Shelley w
    (_, txs) <- unsafeRequest @[ApiTransaction n] ctx ep Empty
    view (#status . #getApiT) <$> txs `shouldSatisfy` all (== InLedger)

-- | An arbitrary but valid pool id, to avoid having to list pools for testing
-- bad requests and headers.
arbitraryPoolId :: ApiT PoolId
arbitraryPoolId = either (error . show) ApiT $ fromText
    "a659052d84ddb6a04189bee523d59c0a3385c921f43db5dc5de17a4f3f11dc4c"

joinStakePoolWithWalletBalance
    :: forall t n. (n ~ 'Testnet)
    => (Context t)
    -> [Natural]
    -> IO (ApiWallet, ApiStakePool)
joinStakePoolWithWalletBalance ctx balance = do
    w <- fixtureWalletWith ctx balance
    (_, p:_) <- eventually "Stake pools are listed in joinStakePoolWithWalletBalance" $
        unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
    r <- joinStakePool ctx (p ^. #id) (w, "Secure Passphrase")
    expectResponseCode HTTP.status202 r
    -- Verify the certificate was discovered
    eventually "Tx in ledger in joinStakePoolWithWalletBalance" $ do
        let ep = Link.listTransactions @'Shelley w
        request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
            [ expectListField 0 (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectListField 0 (#status . #getApiT) (`shouldBe` InLedger)
            ]
    return (w, p)

joinStakePoolWithFixtureWallet
    :: forall t n. (n ~ 'Testnet)
    => (Context t)
    -> IO (ApiWallet, ApiStakePool)
joinStakePoolWithFixtureWallet ctx = do
    w <- fixtureWallet ctx
    (_, p:_) <- eventually "Stake pools are listed in joinStakePoolWithFixtureWallet" $
        unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
    r <- joinStakePool ctx (p ^. #id) (w, fixturePassphrase)
    expectResponseCode HTTP.status202 r
    -- Verify the certificate was discovered
    eventually "Tx in ledger in joinStakePoolWithFixtureWallet" $ do
        let ep = Link.listTransactions @'Shelley w
        request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
            [ expectListField 0 (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectListField 0 (#status . #getApiT) (`shouldBe` InLedger)
            ]
    return (w, p)

getSlotParams
    :: (Context t)
    -> IO (EpochNo, SlotParameters)
getSlotParams ctx = do
    r2 <- request @ApiNetworkInformation ctx
          Link.getNetworkInfo Default Empty
    let (ApiT currentEpoch) = getFromResponse (#networkTip . #epochNumber) r2

    let endpoint = ( "GET", "v2/network/parameters/latest" )
    r3 <- request @ApiNetworkParameters ctx endpoint Default Empty
    let (Quantity slotL) = getFromResponse #slotLength r3
    let (Quantity epochL) = getFromResponse #epochLength r3
    let (Quantity coeff) = getFromResponse #activeSlotCoefficient r3
    let (ApiT genesisBlockDate) = getFromResponse #blockchainStartTime r3
    let sp = SlotParameters (EpochLength epochL) (SlotLength slotL) genesisBlockDate (ActiveSlotCoefficient coeff)

    return (currentEpoch, sp)

placeholder :: BackwardCompatPlaceholder (Identity (ApiT PoolId))
placeholder = Placeholder

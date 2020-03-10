{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Unit tests for 'withShutdownHandler' using pipes within a single process.

module Cardano.StartupSpec
    ( spec
    ) where

import Prelude

import Cardano.Startup
    ( ShutdownHandlerLog (..), withShutdownHandler' )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( race )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Exception
    ( IOException, bracket, catch, throwIO )
import Control.Tracer
    ( Tracer, nullTracer )
import System.IO
    ( Handle, IOMode (..), hClose, stdin, withFile )
import System.IO.Error
    ( isUserError )
import System.Process
    ( createPipe )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldContain, shouldReturn, shouldThrow )
import Test.Utils.Trace
    ( captureLogging )
import Test.Utils.Windows
    ( nullFileName )

import qualified Data.ByteString as BS

spec :: Spec
spec = describe "withShutdownHandler" $ do
    let decisecond = 100000

    describe "sanity tests" $ do
        -- read file handle, allowing interruptions on windows
        let getChunk :: Handle -> IO BS.ByteString
            getChunk h = do
                v <- newEmptyMVar
                (BS.hGet h 1000 >>= putMVar v . Right) `catch` (putMVar v . Left)
                takeMVar v >>= \case
                  Left (e :: IOException)  -> throwIO e
                  Right bs -> pure bs

        it "race stdin" $ do
            res <- race (getChunk stdin) (threadDelay decisecond)
            res `shouldBe` Right ()

        it "race pipe" $ withPipe $ \(a, _) -> do
            res <- race (getChunk a) (threadDelay decisecond)
            res `shouldBe` Right ()

    it "action completes immediately" $ withPipe $ \(a, _) -> do
        logs <- captureLogging' $ \tr -> do
            withShutdownHandler' tr a (pure ())
                `shouldReturn` Just ()
        logs `shouldContain` [MsgShutdownHandler True]

    it "action completes with delay" $ withPipe $ \(a, _) -> do
        res <- withShutdownHandler' nullTracer a $ do
            threadDelay decisecond
            pure ()
        res `shouldBe` Just ()

    it "handle is closed immediately" $ withPipe $ \(a, b) -> do
        logs <- captureLogging' $ \tr -> do
            res <- withShutdownHandler' tr a $ do
                hClose b
                threadDelay decisecond -- give handler a chance to run
                pure ()
            res `shouldBe` Nothing
        logs `shouldContain` [MsgShutdownEOF]

    it "handle is closed with delay" $ withPipe $ \(a, b) -> do
        res <- withShutdownHandler' nullTracer a $ do
            threadDelay decisecond
            hClose b
            threadDelay decisecond -- give handler a chance to run
            pure ()
        res `shouldBe` Nothing

    it "action throws exception" $ withPipe $ \(a, _) -> do
        let bomb = userError "bomb"
        logs <- captureLogging' $ \tr -> do
            withShutdownHandler' tr a (throwIO bomb)
                `shouldThrow` isUserError
        logs `shouldBe` [MsgShutdownHandler True]

    it ("handle is " ++ nullFileName ++ " (immediate EOF)") $ do
        logs <- captureLogging' $ \tr ->
            withFile nullFileName ReadMode $ \h -> do
                res <- withShutdownHandler' tr h $ do
                    threadDelay decisecond -- give handler a chance to run
                    pure ()
                res `shouldBe` Nothing
        logs `shouldContain` [MsgShutdownEOF]

    it "handle is already closed" $ withPipe $ \(a, b) -> do
        hClose a
        hClose b
        logs <- captureLogging' $ \tr -> do
            res <- withShutdownHandler' tr a $ do
                threadDelay decisecond
                hClose b
                threadDelay decisecond -- give handler a chance to run
                pure ()
            res `shouldBe` Just ()
        logs `shouldContain` [MsgShutdownHandler False]

withPipe :: ((Handle, Handle) -> IO a) -> IO a
withPipe = bracket createPipe closePipe
    where closePipe (a, b) = hClose a >> hClose b

captureLogging' :: (Tracer IO msg -> IO a) -> IO [msg]
captureLogging' = fmap fst . captureLogging

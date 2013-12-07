{-# LANGUAGE OverloadedStrings #-}
module Helper (
  module Test.Hspec
, module Control.Applicative
, module Network.HTTP.Types
, Req (..)
, withHttpServer
) where

import           Test.Hspec
import           Control.Applicative
import           Control.Exception
import           Control.Concurrent
import           Data.IORef
import           Control.Monad.IO.Class (liftIO)
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Data.ByteString.Lazy
import           Data.Conduit.Lazy
import           Control.DeepSeq

data Req = Req {
  reqMethod :: Method
, reqHeaders :: RequestHeaders
, reqBody :: ByteString
} deriving (Eq, Show)

toReq :: Request -> IO Req
toReq r = do
  b <- body r
  b `deepseq` (return $ Req (requestMethod r) (requestHeaders r) b)
  where
    body :: Request -> IO ByteString
    body = fmap fromChunks . lazyConsume . requestBody

withHttpServer :: Status -> ResponseHeaders -> ByteString -> (IO Req -> IO a) -> IO a
withHttpServer status headers body action = do
  ref <- newIORef (error "No request received!")
  withApplication (app ref) $ do
    action (readIORef ref)
  where
    app :: IORef Req -> Application
    app ref request = do
      liftIO $ toReq request >>= writeIORef ref
      return (responseLBS status headers body)

withApplication :: Application -> IO a -> IO a
withApplication app action = do
  started <- newEmptyMVar
  stopped <- newEmptyMVar
  threadId <- forkIO $
    runSettings defaultSettings {settingsPort = 3000, settingsBeforeMainLoop = putMVar started ()} app `finally` putMVar stopped ()
  takeMVar started
  action `finally` (killThread threadId >> takeMVar stopped)

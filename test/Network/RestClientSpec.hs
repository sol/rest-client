{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Network.RestClientSpec (main, spec) where

import           Helper

import           Network.RestClient

main :: IO ()
main = hspec spec

withServer :: (IO Req -> IO a) -> IO a
withServer = withHttpServer status200 [("Content-Type", "text/plain")] "OK"

spec :: Spec
spec = do
  describe "get" $ do
    it "performs a GET request" $ withServer $ \r -> do
      get "http://localhost:3000"
      reqMethod <$> r `shouldReturn` "GET"

    it "returns server response" $ withServer $ \_ -> do
      get "http://localhost:3000" `shouldReturn` "OK"

  describe "post" $ do
    it "performs a POST request" $ withServer $ \r -> do
      post "http://localhost:3000" ""
      reqMethod <$> r `shouldReturn` "POST"

    it "attaches a body to the request" $ withServer $ \r -> do
      post "http://localhost:3000" "foobar"
      reqBody <$> r `shouldReturn` "foobar"

    it "returns server response" $ withServer $ \_ -> do
      post "http://localhost:3000" "" `shouldReturn` "OK"

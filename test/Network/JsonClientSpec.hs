{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Network.JsonClientSpec (main, spec) where

import           Helper

import           Network.JsonClient

main :: IO ()
main = hspec spec

withServer :: (IO Req -> IO a) -> IO a
withServer = withHttpServer status200 [("Content-Type", "application/json")] "[10, 20, 30]"

spec :: Spec
spec = do
  describe "get" $ do
    let action = get "http://localhost:3000" :: IO (Either String [Int])

    it "performs a GET request" $ withServer $ \r -> do
      action
      reqMethod <$> r `shouldReturn` "GET"

    it "returns server response" $ withServer $ \_ -> do
      action `shouldReturn` Right [10, 20, 30]

  describe "post" $ do
    let action = post "http://localhost:3000" [23, 42 :: Int] :: IO (Either String [Int])

    it "performs a POST request" $ withServer $ \r -> do
      action
      reqMethod <$> r `shouldReturn` "POST"

    it "attaches a body to the request" $ withServer $ \r -> do
      action
      reqBody <$> r `shouldReturn` "[23,42]"

    it "sets Concurrent-Type to application/json" $ withServer $ \r -> do
      action
      r >>= (`shouldSatisfy` ((elem ("Content-Type", "application/json")) .  reqHeaders))

    it "returns server response" $ withServer $ \_ -> do
      action `shouldReturn` (Right [10, 20, 30] :: Either String [Int])

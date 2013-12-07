{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Network.JsonClient where

import           Control.Applicative
import           Control.Failure
import           Control.Monad.IO.Class
import           Network.HTTP.Client
import           Data.Aeson hiding (decode, decode', (.:))
import           Data.Aeson.Toolkit

import           Network.RestClient as Http

get :: (Http m, Failure String f, FromJSON a) => String -> m (f a)
get = fmap decode . Http.get

post :: (Http m, Failure String f, ToJSON a, FromJSON b) => String -> a -> m (f b)
post url body = do
  req <- liftIO $ parseUrl url
  decode . responseBody <$> http req {method = "POST", requestBody = RequestBodyLBS (encode body), requestHeaders = [("Content-Type", "application/json")]}

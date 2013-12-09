{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Network.JsonClient (
  Http
, get
, post
) where

import           Control.Applicative
import           Control.Failure
import           Data.Aeson hiding (decode, decode', (.:))
import           Data.Aeson.Toolkit

import           Network.RestClient hiding (get, post)
import qualified Network.RestClient as Http

get :: (Http m, Failure String f, FromJSON a) => String -> m (f a)
get = fmap decode . Http.get

post :: (Http m, Failure String f, ToJSON a, FromJSON b) => String -> a -> m (f b)
post url b = decode . body <$> http (Request "POST" url [("Content-Type", "application/json")] (encode b))

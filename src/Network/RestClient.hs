{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Network.RestClient where

import           Control.Applicative
import qualified Control.Exception as E
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy as L
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

get :: Http m => String -> m L.ByteString
get url = do
  req <- liftIO $ parseUrl url
  responseBody <$> http req

post :: Http m => String -> L.ByteString -> m L.ByteString
post url b = do
  req <- liftIO $ parseUrl url
  responseBody <$> http req {method = "POST", requestBody = RequestBodyLBS b}

class (Functor m, Applicative m, Monad m, MonadIO m) => Http m where
  http :: Request -> m (Response L.ByteString)

instance Http IO where
  http = runHttp . http

newtype HttpM a = HttpM (ReaderT Manager IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance Http HttpM where
  http req = HttpM $ do
    manager <- ask
    liftIO $ httpLbs req manager

runHttp :: HttpM a -> IO a
runHttp (HttpM action) = E.bracket (newManager tlsManagerSettings) closeManager (runReaderT action)

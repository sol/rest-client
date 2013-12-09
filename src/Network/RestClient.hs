{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Network.RestClient (
  Request(..)
, Response(..)
, Http(..)
, runHttp
, get
, post
) where

import           Control.Applicative
import qualified Control.Exception as E
import           Control.Failure
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy as L
import           Data.Default
import           Network.HTTP.Types
import qualified Network.HTTP.Client as C
import           Network.HTTP.Client.TLS

get :: Http m => String -> m L.ByteString
get url = body <$> http (Request "GET" url [] "")

post :: Http m => String -> L.ByteString -> m L.ByteString
post url b = body <$> http (Request "POST" url [] b)

class (Functor m, Applicative m, Monad m, MonadIO m) => Http m where
  http :: Request -> m Response

instance Http IO where
  http = runHttp . http

newtype HttpM a = HttpM (ReaderT C.Manager IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance Http HttpM where
  http req = HttpM $ do
    r <- toClientRequest req
    manager <- ask
    liftIO $ fromClientResponse <$> C.httpLbs r manager

toClientRequest :: Failure C.HttpException m => Request -> m C.Request
toClientRequest (Request m url hs b) = do
  req <- C.parseUrl url
  return req {C.method = m, C.requestHeaders = hs, C.requestBody = C.RequestBodyLBS b}

fromClientResponse :: C.Response L.ByteString -> Response
fromClientResponse r = Response (C.responseStatus r) (C.responseHeaders r) (C.responseBody r)

runHttp :: HttpM a -> IO a
runHttp (HttpM action) = E.bracket (C.newManager tlsManagerSettings) C.closeManager (runReaderT action)

data Request = Request {
  requestMethod  :: Method
, requestUrl     :: String
, requestHeaders :: RequestHeaders
, requestBody    :: L.ByteString
} deriving (Eq, Ord, Show)

instance Default Request where
  def = Request "GET" "http://example.com/" [] ""

data Response = Response {
  status  :: Status
, headers :: ResponseHeaders
, body    :: L.ByteString
} deriving (Eq, Show)


instance Default Response where
  def = Response status200 [] ""

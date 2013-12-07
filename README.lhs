### Example

We need some language extensions an imports:

~~~ {.haskell}
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
import Network.JsonClient
import Data.Aeson hiding ((.:))
import Data.Aeson.Toolkit
import Control.Monad
import Control.Exception
~~~


Next, we make `String` and instance of `Exception`, so that `Failure String m`
can be unified to `IO`:
~~~ {.haskell}
-- Don't try this at home!
instance Exception String where
  toException = SomeException . ErrorCall
  fromException e = case fromException e of
    Just (ErrorCall err) -> Just err
    Nothing -> Nothing
~~~

Finally, we shorten a URL with the [Google URL Shortener API](https://developers.google.com/url-shortener/v1/getting_started).
~~~ {.haskell}
url :: String
url = "https://www.googleapis.com/urlshortener/v1/url"

main :: IO ()
main = do
  r <- join $ post url $ object ["longUrl" .= ("http://www.google.com/" :: String)]
  r .: "id" >>= putStrLn
~~~

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Data.Text hiding (words)
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as BS
import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import Network.HTTP.Types.Status (statusCode)

newtype Tweet = Tweet
    { text :: Text
    } deriving (Show, Generic)

instance FromJSON Tweet
instance ToJSON Tweet

appName = "AkariApp"

getMyOauth = do
    [key,secret] <- BS.words <$> BS.readFile "info/oauth"
    BS.putStrLn key
    BS.putStrLn secret
    return $ newOAuth
        { oauthServerName = "api.twitter.com"
        , oauthConsumerKey = key
        , oauthConsumerSecret = secret
        }

getMyCredential = do
    [token, secret_token] <- BS.words <$> BS.readFile "info/credential"
    BS.putStrLn token
    BS.putStrLn secret_token
    return $ newCredential token secret_token

tweet :: Text -> IO ()
tweet t = do
    _oauth <- getMyOauth
    _credential <- getMyCredential
    req <- parseRequest "https://api.twitter.com/1.1/statuses/update.json"
    manager <- newManager tlsManagerSettings
    postReq <- signOAuth _oauth _credential $
                urlEncodedBody [("status",encodeUtf8 t)] req
    res <- httpLbs postReq manager
    putStrLn $ show $ statusCode $ responseStatus res
    return ()

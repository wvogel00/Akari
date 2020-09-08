{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Data.Text hiding (words, length)
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64 (encode)
import Data.Maybe
import Control.Lens
import Data.Aeson.Lens
import GHC.Generics
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import Network.HTTP.Types.Status (statusCode)
import Data.Time.LocalTime hiding (Day)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Control.Concurrent (threadDelay)
import qualified Akari.Yaml as A
import qualified Akari.Time as AT

data Tweet = Tweet
    { text :: Text
    , img :: Maybe Text
    } deriving (Show, Generic)

appName = "AkariApp"
isDayTime (_,_,h) = 7 <= h && h <= 19

isAbleToTweet :: AT.Date -> AT.Date -> A.Tweet -> Bool
isAbleToTweet old now tw = A.from tw < now
                        && now < A.to tw
                        && A.span tw < AT.clock (now-old)

signAkari = flip append "\n\n -Akariが投稿しています-"


getJSTTime :: IO AT.Date
getJSTTime = getTimeInfo <$> getZonedTime

getTimeInfo t = AT.Date y m d (AT.Clock h mn) where
    y = read $ formatTime defaultTimeLocale "%Y" t
    m = read $ formatTime defaultTimeLocale "%m" t
    d = read $ formatTime defaultTimeLocale "%d" t
    h = read $ formatTime defaultTimeLocale "%H" t
    mn = read $ formatTime defaultTimeLocale "%M" t

autoTweetYml :: FilePath -> IO ()
autoTweetYml ymlfile = do
    yml <- A.readYaml "settings/tweet.yml"
    case yml of
        Nothing -> putStrLn $ ymlfile ++ "が存在しないため実行できません"
        Just rs -> do
            putStrLn $ show (length $ A.tweet rs) ++ "件の予約を並列実行します"
            mapM_ tweetReserved $ A.tweet rs

tweetReserved :: A.Tweet -> IO()
tweetReserved tw = do
    now <- getJSTTime
    tweetReserved' now tw

tweetReserved' :: AT.Date -> A.Tweet -> IO ()
tweetReserved' old tw = do
    now <- getJSTTime
    if isAbleToTweet old now tw
        then do
            tweet (A.contents tw) (A.media tw)
            tweetReserved' now tw
        else do
            threadDelay $ 60 * 10^6 -- 1 minute
            tweetReserved' old tw

getMyOauth = do
    [key,secret] <- BS.words <$> BS.readFile "info/oauth"
    return $ newOAuth
        { oauthServerName = "api.twitter.com"
        , oauthConsumerKey = key
        , oauthConsumerSecret = secret
        }

getMyCredential = do
    [token, secret_token] <- BS.words <$> BS.readFile "info/credential"
    return $ newCredential token secret_token

tweet :: Text -> Maybe Text -> IO ()
tweet t file = do
    _oauth <- getMyOauth
    _credential <- getMyCredential
    contents <- makeContents t file
    req <- parseRequest "https://api.twitter.com/1.1/statuses/update.json"
    manager <- newManager tlsManagerSettings
    postReq <- signOAuth _oauth _credential $
                urlEncodedBody contents req
    res <- httpLbs postReq manager
    -- putStrLn $ show $ statusCode $ responseStatus res
    return ()

makeContents t file = case file of
    Nothing -> return [t']
    Just path -> do
        ids <- upload $ unpack path
        return [t', ("media_ids", encodeUtf8 ids)]
    where
        t' = ("status",encodeUtf8 $ signAkari t)

-- 画像をuploadし，"media_id_strings"フィールドの文字列を返す
upload :: String -> IO Text
upload path = do
    img <- B64.encode <$> B.readFile path
    _oauth <- getMyOauth
    _credential <- getMyCredential
    req <- parseRequest "https://upload.twitter.com/1.1/media/upload.json"
    manager <- newManager tlsManagerSettings
    postReq <- signOAuth _oauth _credential $ urlEncodedBody [("media_data",img)] req
    res <- httpLbs postReq manager
    return $ responseBody res ^. key "media_id_string" . _String

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Data.Text hiding (words)
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

data Tweet = Tweet
    { text :: Text
    , img :: Maybe Text
    } deriving (Show, Generic)

appName = "AkariApp"

type TimeInfo = (Month, Day, Hour)
type Month = Int
type Day = Int
type Hour = Int

hour (_,_,h) = h
diff (9,d1,h1) (8,d2,h2) = h1-h2+24
diff (_,d1,h1) (_,d2,h2) = (d1-d2)*24 + h1-h2

isDayTime (_,_,h) = 7 <= h && h <= 19

inOnTime :: A.Date -> A.Tweet -> Bool
inOnTIme d tw = A.from tw < d && d < A.to tw

signAkari = flip append "\n\n -Akariが投稿しています-"


getJSTTime :: IO TimeInfo
getJSTTime = getTimeInfo <$> getZonedTime

getTimeInfo t = (read m,read d,read h) :: TimeInfo where
    m = formatTime defaultTimeLocale "%m" t
    d = formatTime defaultTimeLocale "%d" t
    h = formatTime defaultTimeLocale "%H" t

autoTweetYml :: FilePath -> IO ()
autoTweetYml ymlfile = do
    yml <- A.readYaml "settings/tweet.yml"
    case yml of
        Nothing -> putStrLn $ ymlfile ++ "が存在しないため実行できません"
        Just rs -> do
            putStrLn $ show (length $ tweet rs) ++ "件の予約を並列実行します"
            mapM_ tweetReserved $ tweet rs

tweetReserved :: A.Tweet -> IO()
tweetReserved tw = do
    threadDelay $ 60 * 10^6 -- 60secおき
    autoTweetYml

autoTweet :: Maybe TimeInfo -> IO ()
autoTweet Nothing = do
    -- tweet $ Tweet {text = "自動投稿を開始します"}
    now <- getJSTTime
    autoTweet =<< Just <$> getJSTTime
autoTweet (Just old) = do
    now <- getJSTTime
    if isDayTime now && now `diff` old >= 2 && odd (hour now)
            then do
                let state = stateAt now
                -- tweet $ Tweet {text=tweetAt state, img = imageAt state }
                putStr "now -> " >> print now
                autoTweet (Just now)
            else do
                threadDelay $ 10 * 10^6 -- micro sec. 10sec毎に実行
                putStr "old -> " >> print old
                autoTweet (Just old)

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

tweet :: Tweet -> IO ()
tweet tw = do
    _oauth <- getMyOauth
    _credential <- getMyCredential
    contents <- makeContents tw 
    req <- parseRequest "https://api.twitter.com/1.1/statuses/update.json"
    manager <- newManager tlsManagerSettings
    postReq <- signOAuth _oauth _credential $
                urlEncodedBody contents req
    res <- httpLbs postReq manager
    -- putStrLn $ show $ statusCode $ responseStatus res
    return ()

makeContents tw = case img tw of
    Nothing -> return [("status",encodeUtf8 . signAkari $ text tw)]
    Just path -> do
        ids <- upload $ unpack path
        return [("status",encodeUtf8 . signAkari $ text tw), ("media_ids", encodeUtf8 ids)]

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

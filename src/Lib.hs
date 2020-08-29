{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Data.Text hiding (words)
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as B
import Data.Maybe
import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import Network.HTTP.Types.Status (statusCode)
import Data.Time.LocalTime hiding (Day)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Control.Concurrent (threadDelay)

newtype Tweet = Tweet
    { text :: Text
    } deriving (Show, Generic)

instance FromJSON Tweet
instance ToJSON Tweet

appName = "AkariApp"

logoPath = "image/logo.png"

type TimeInfo = (Month, Day, Hour)
type Month = Int
type Day = Int
type Hour = Int

hour (_,_,h) = h
diff (8,d1,h1) (8,d2,h2) = h2-h1
diff (8,d1,h1) (9,d2,h2) = h2-h1+24
diff (9,d1,h1) (9,d2,h2) = h2-h1

isDayTime (_,_,h) = 9 <= h && h <= 19

akariSign = "\n\n -Akariからの自動投稿です-"

tweetAt (m,d,h)
    | (m,d) == (8,31) && h == 9 = append "手術開始です！" akariSign
    | (m,d) == (8,31) && h < 18 = append "手術中です" akariSign
    | (m,d) == (8,31) && otherwise = append "ICUにいます" akariSign
    | m == 9 && d == 1 = append "ICUにいます" akariSign
    | m == 9 && d == 2 && h <= 12 = append "多分ICUにいます" akariSign
    | m == 9 = append "ICUか重症室にいます...多分..." akariSign
    | otherwise = append "Akariには主人の居場所がわかりません！" akariSign

getJSTTime :: IO TimeInfo
getJSTTime = getTimeInfo <$> getZonedTime

getTimeInfo t = (read m,read d,read h) :: TimeInfo where
    m = formatTime defaultTimeLocale "%m" t
    d = formatTime defaultTimeLocale "%d" t
    h = formatTime defaultTimeLocale "%H" t

autoTweet :: Maybe TimeInfo -> IO ()
autoTweet Nothing = do
    -- tweet $ Tweet {text = "自動投稿を開始します"}
    putStrLn "test"
    now <- getJSTTime
    autoTweet =<< Just <$> getJSTTime
autoTweet (Just old) = do
    now <- getJSTTime
    if isDayTime now && now `diff` old >= 2 && odd (hour now)
            then do
                -- tweet $ Tweet {text=tweetAt now}
                putStrLn "test"
                autoTweet (Just now)
            else do
                threadDelay $ 10 * 10^6 -- micro sec. 10sec毎に実行
                autoTweet (Just old)
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

tweet :: Tweet -> IO ()
tweet tw = do
    _oauth <- getMyOauth
    _credential <- getMyCredential
    req <- parseRequest "https://api.twitter.com/1.1/statuses/update.json"
    manager <- newManager tlsManagerSettings
    postReq <- signOAuth _oauth _credential $
                urlEncodedBody [("status",encodeUtf8 $ text tw)] req
    res <- httpLbs postReq manager
    putStrLn $ show $ statusCode $ responseStatus res
    return ()

makeContents tw img = do
    case img of
        Nothing -> return [("status",encodeUtf8 $ text tw)]
        Just path -> do
            ids <- upload $ path
            return [("status",encodeUtf8 $ text tw), ("media_ids", ids)]

-- 画像をuploadし，"media_ids"フィールドの文字列を返す
upload :: String -> IO B.ByteString
upload path = do
    _oauth <- getMyOauth
    _credential <- getMyCredential
    req <- parseRequest "https://api.twitter.com/1.1/statuses/update.json"
    manager <- newManager tlsManagerSettings
    postReq <- signOAuth _oauth _credential $
                urlEncodedBody [("media","tet")] req
    res <- httpLbs postReq manager
    putStrLn $ show $ statusCode $ responseStatus res
    return "test"
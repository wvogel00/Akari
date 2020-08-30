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

data Tweet = Tweet
    { text :: Text
    , img :: Maybe Text
    } deriving (Show, Generic)

appName = "AkariApp"

logoPath = "image/logo.png"

type TimeInfo = (Month, Day, Hour)
type Month = Int
type Day = Int
type Hour = Int

data State = OpeStart | OnOpe | ICU | SoCritical | Critical | UnKnown | Nominal
    deriving (Eq,Show)

hour (_,_,h) = h
diff (9,d1,h1) (8,d2,h2) = h1-h2+24
diff (_,d1,h1) (_,d2,h2) = (d1-d2)*24 + h1-h2

isDayTime (_,_,h) = 7 <= h && h <= 19

signAkari = flip append "\n\n -Akariが投稿しています-"

tweetAt state = case state of
    Nominal     -> "Akariは待機中です！Akariは日中，二時間に一回，主人の様子をお伝えします．"
    OpeStart    -> "手術開始です！手術成功しなくても良いから無事に戻ってきますように！"
    OnOpe       -> "手術中です...そわそわ"
    ICU         -> "ICUにいます"
    SoCritical  -> "多分ICUにいます..重症室へ移動したのでしょうか"
    Critical    -> "ICUか重症室にいます...多分..."
    UnKnown     -> "Akariには主人の居場所がわかりません！ピンチです！"

imageAt state = case state of
    Nominal     -> Nothing
    OpeStart    -> Just "image/abstract.png"
    OnOpe       -> Just "image/onope.png"
    ICU         -> Just "image/icu.png"
    SoCritical  -> Nothing
    Critical    -> Nothing
    UnKnown     -> Nothing

stateAt (m,d,h)
    | (m,d)==(8,30) || (m == 8 && d == 31 && h <= 8) = Nominal
    | (m,d) == (8,31) && h == 9 = OpeStart
    | (m,d) == (8,31) && h < 18 = OnOpe
    | (m,d) == (8,31) && otherwise = ICU
    | m == 9 && d == 1 = ICU
    | m == 9 && d == 2 && h <= 12 = SoCritical
    | m == 9 = Critical
    | otherwise = UnKnown

getJSTTime :: IO TimeInfo
getJSTTime = getTimeInfo <$> getZonedTime

getTimeInfo t = (read m,read d,read h) :: TimeInfo where
    m = formatTime defaultTimeLocale "%m" t
    d = formatTime defaultTimeLocale "%d" t
    h = formatTime defaultTimeLocale "%H" t

autoTweet :: Maybe TimeInfo -> IO ()
autoTweet Nothing = do
    tweet $ Tweet {text = "自動投稿を開始します"}
    now <- getJSTTime
    autoTweet =<< Just <$> getJSTTime
autoTweet (Just old) = do
    now <- getJSTTime
    if isDayTime now && now `diff` old >= 2 -- && odd (hour now)
            then do
                let state = stateAt now
                tweet $ Tweet {text=tweetAt state, img = imageAt state }
                -- putStr "now -> " >> print now
                autoTweet (Just now)
            else do
                threadDelay $ 10 * 10^6 -- micro sec. 10sec毎に実行
                -- putStr "old -> " >> print old
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

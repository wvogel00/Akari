{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Akari.Yaml (readYaml) where

import GHC.Generics
import qualified Data.Yaml as Y (Parser)
import Data.Yaml
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Functor
import Debug.Trace

data Reserved = Reserved {tweet :: [Tweet]} deriving (Show, Generic)

data Tweet = Tweet 
    { from :: Date
    , to :: Date
    , stopfrom :: Maybe String
    , stopto :: Maybe String
    , span :: String
    , contents :: T.Text
    , media :: Maybe T.Text
    } deriving (Show, Generic)

instance FromJSON Reserved
--instance FromJSON Tweet

data Date = Date
    { year :: Int
    , month :: Int
    , day :: Int
    , hour :: Int
    , minute :: Int
    } deriving (Show, Ord, Eq)

instance FromJSON Tweet where
    parseJSON (Object o) = do
        v <- o.: "stopto"
        trace ("\n\n\n\n" ++  v) $
            Tweet <$> (readDate <$> o .: "from")
                  <*> (readDate <$> o .: "to")
                  <*> (o .: "stopfrom")
                  <*> (o .: "stopto")
                  <*> (o .: "span")
                  <*> (o .: "contents")
                  <*> (o .: "media")

readDate str = Date {year = y, month = m, day = d, hour = h, minute = mn}
    where
        [mn,h,d,m,y] = if elem '/' str
            then readDate' str
            else 0:0:readDate' str
        readDate' = map read . filter (not.null) . readDate'' ([],[])

readDate'' :: (String, [String]) -> String -> [String]
readDate'' (v,vs) [] = v:vs
readDate'' (v,vs) (x:xs) = if elem x (":/-" :: String)
    then readDate'' ([],v:vs) xs
    else readDate'' (v++[x],vs) xs

readYaml :: IO (Maybe Reserved)
readYaml = do
    cs <- BS.readFile "settings/tweet.yml"
    let rs = decode cs :: Maybe Reserved
    case rs of
        Nothing -> BS.putStrLn "読み込みに失敗しました" $> Nothing
        Just _ -> return rs

length :: Reserved -> Int
length (Reserved xs) = Prelude.length xs

(!!) (Reserved rs) = (Prelude.!!) rs
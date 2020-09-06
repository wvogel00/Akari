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

data Date = Date
    { year :: Int
    , month :: Int
    , day :: Int
    , hour :: Int
    , minute :: Int
    } deriving (Show, Ord, Eq)

instance FromJSON Reserved

instance FromJSON Tweet where
    parseJSON (Object o) = 
            Tweet <$> (readDate <$> o .: "from")
                  <*> (readDate <$> o .: "to")
                  <*> (o .:? "stopfrom")
                  <*> (o .:? "stopto")
                  <*> (o .: "span")
                  <*> (o .: "contents")
                  <*> (o .:? "media")

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
    let rs = decodeEither' cs :: Either ParseException Reserved
    case rs of
        Left NonScalarKey -> putStrLn "数値がありません" $> Nothing
        Left (UnknownAlias name) -> putStrLn ("不明なエイリアス" ++ name ++ "があります") $> Nothing
        Left MultipleDocuments -> putStrLn "ファイルが複数あります" $> Nothing
        Left (InvalidYaml _) -> putStrLn "yamlが不正です" $> Nothing
        Left (UnexpectedEvent _ _) -> putStrLn ("予期せぬイベントがありました") $> Nothing
        Left (AesonException str) -> putStrLn ("aeson例外です:" ++ str) $> Nothing
        Left (OtherParseException _) -> putStrLn "パース例外が発生しました" $> Nothing
        Left (NonStringKey jpath) -> putStrLn (show jpath ++ "がありません") $> Nothing
        Left (NonStringKeyAlias name v) -> putStrLn ("キーエイリアス"++name++"がありません") $> Nothing
        Left CyclicIncludes -> putStrLn "繰り返し設定されている値があります" $> Nothing
        Left (LoadSettingsException path _) -> putStrLn (path ++ "がありません") $> Nothing
        Right rs -> return $ Just rs

length :: Reserved -> Int
length (Reserved xs) = Prelude.length xs

(!!) (Reserved rs) = (Prelude.!!) rs


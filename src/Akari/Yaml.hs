{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Akari.Yaml where

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
    } deriving (Ord, Eq)

instance FromJSON Reserved

dayCount m
    | elem m [1,3,5,7,8,10,12] = 31
    | elem m [4,6,9,11] = 30
    | m == 2 = 28

instance Show Date where
    show d = (show $ year d) ++ "-" ++ (show $ month d) ++ "-" ++ (show $ day d)
                ++ "/" ++ (show $ hour d) ++ ":" ++ (show $ minute d)

instance Num Date where
    a + b = undefined
    a * b = undefined
    negate = undefined
    abs a = a
    signum = undefined
    fromInteger = undefined
    a - b = Date { year = year'
                 , month = 12*year' + month'
                 , day = day'
                 , hour = hour' `mod` 24
                 , minute = minute' `mod` 60
                 } where
                     minute' = minute a - minute b
                     hour' = if minute' < 0 then hour a - hour b - 1 else hour a - hour b
                     year' = year a - year b
                     day' = if month a > month b then day a - day b + dayCount (month b) else day a - day b
                     month' = if day a < day b then month a - month b - 1 else month a - month b


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

readYaml :: FilePath -> IO (Maybe Reserved)
readYaml file = do
    cs <- BS.readFile file
    let rs = decodeEither' cs :: Either ParseException Reserved
    case rs of
        Left  NonScalarKey                  -> Nothing <$ putStrLn "数値がありません"
        Left (UnknownAlias name)            -> Nothing <$ putStrLn ("不明なエイリアス" ++ name ++ "があります")
        Left  MultipleDocuments             -> Nothing <$ putStrLn "ファイルが複数あります"
        Left (InvalidYaml _)                -> Nothing <$ putStrLn "yamlが不正です"
        Left (UnexpectedEvent _ _)          -> Nothing <$ putStrLn ("予期せぬイベントがありました")
        Left (AesonException str)           -> Nothing <$ putStrLn ("aeson例外です:" ++ str)
        Left (OtherParseException _)        -> Nothing <$ putStrLn "パース例外が発生しました"
        Left (NonStringKey jpath)           -> Nothing <$ putStrLn (show jpath ++ "がありません")
        Left (NonStringKeyAlias name v)     -> Nothing <$ putStrLn ("キーエイリアス"++name++"がありません")
        Left  CyclicIncludes                -> Nothing <$ putStrLn "繰り返し設定されている値があります"
        Left (LoadSettingsException path _) -> Nothing <$ putStrLn (path ++ "がありません")
        Right rs -> return $ Just rs
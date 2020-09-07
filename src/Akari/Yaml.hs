{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Akari.Yaml where

import Akari.Time
import GHC.Generics
import qualified Data.Yaml as Y (Parser)
import Data.Yaml
    ( FromJSON(parseJSON),
      (.:),
      (.:?),
      Value(Object),
      decodeEither',
      ParseException(LoadSettingsException, NonScalarKey, UnknownAlias,
                     MultipleDocuments, InvalidYaml, UnexpectedEvent, AesonException,
                     OtherParseException, NonStringKey, NonStringKeyAlias,
                     CyclicIncludes) )
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Functor
import Data.Maybe
import Debug.Trace

data Reserved = Reserved {tweet :: [Tweet]} deriving (Show, Generic)

data Tweet = Tweet 
    { from :: Date
    , to :: Date
    , stopfrom :: Maybe Clock
    , stopto :: Maybe Clock
    , span :: Clock
    , contents :: T.Text
    , media :: Maybe T.Text
    } deriving (Show, Generic)

instance FromJSON Reserved

instance FromJSON Tweet where
    parseJSON (Object o) = 
            Tweet <$> (readDate <$> o .: "from")
                  <*> (readDate <$> o .: "to")
                  <*> (readClock <$> o .:? "stopfrom")
                  <*> (readClock <$> o .:? "stopto")
                  <*> (fromJust . readClock . Just <$> o .: "span")
                  <*> (o .: "contents")
                  <*> (o .:? "media")

readDate str = Date y m d $ Clock h mn
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

readClock Nothing = Nothing
readClock (Just str) = Just $ Clock h m
    where
        (h,m) = split ':' str
        split c = split' c ""
        split' c xs [] = (read xs,0)
        split' c xs (y:ys) = if c == y then (read xs,read ys) else split' c (xs++[y]) ys

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
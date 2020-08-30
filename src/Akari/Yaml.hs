{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Akari.Yaml (readYaml) where

import GHC.Generics
import Data.Yaml
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Functor

data Reserved = Reserved {tweet :: [Tweet]} deriving (Show, Generic)

data Tweet = Tweet 
    { from :: String
    , to :: String
    , stopfrom :: Maybe String
    , stopto :: Maybe String
    , span :: String
    , contents :: T.Text
    , media :: Maybe T.Text
    } deriving (Show, Generic)

instance FromJSON Reserved
instance FromJSON Tweet

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
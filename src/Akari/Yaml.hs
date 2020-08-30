{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Akari.Yaml (readYaml) where

import GHC.Generics
import Control.Monad.Trans.Class (lift)
import qualified Data.Yaml as Y (Parser)
import Data.Yaml
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Functor
import Control.Applicative
import qualified Text.Trifecta as P (Parser)
import Text.Trifecta
import Text.Trifecta.Delta (Delta(..))

data Reserved = Reserved {tweet :: [Tweet]} deriving (Show, Generic)

data Tweet = Tweet 
    { from :: Date
    , to :: String
    , stopfrom :: Maybe String
    , stopto :: Maybe String
    , span :: String
    , contents :: T.Text
    , media :: Maybe T.Text
    } deriving (Show, Generic)

instance FromJSON Reserved
instance FromJSON Tweet

data Date = Date
    { year :: Int
    , month :: Int
    , day :: Int
    , hour :: Int
    , minute :: Int
    } deriving (Show, Ord, Eq)

{-
newtype FromJSONT v = FromJSONT {runFromJSONT :: P.Parser v -> Y.Parser v}

instance Monad (FromJSONT v) where
    return = lift.return
    m1 >>= m2 = FromJSONT $ \v -> do
                    a <- runFromJSONT (m1 v)
                    runFromJSONT (m2 a)

instance Functor (FromJSONT e) where
  fmap = liftM

instance Applicative (FromJSONT e) where
  pure = return
  (<*>) = ap
-}
instance FromJSON Date where
    parseJSON (String str) = return $ Date
        {year = 1, month = 1, day = 1, hour = 1, minute = 1}
    {-
    parseJSON (String str) = do
        runFromJSONT $ do
            result <- lift $ parseString dateP (Columns 0 0) $ T.unpack str
            case result of
                Success a -> return  a
                Failure e -> return undefined
            where
                dateP = return $ Failure ""
    -}

dateP :: P.Parser Date
dateP = f <$> ymdP <*> try hmP where
    f v Nothing = f v $ Just (0,0)
    f (y,m,d) (Just (h,mn)) = Date {
        year = y, month = m, day = d,
        hour = h, minute = mn
        }

ymdP :: P.Parser (Int,Int,Int)
ymdP = (,,) <$> numP <*> (char '-' *> numP) <*> (char '-' *> numP)

hmP :: P.Parser (Maybe (Int,Int))
hmP = do
    (\a b -> Just (a,b)) <$> (char '/' *> numP) <*> (char ':' *> numP)
    <|> return Nothing

numP :: P.Parser Int
numP = read <$> many digit

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
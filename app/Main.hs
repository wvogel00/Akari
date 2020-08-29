{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Data.Text

main :: IO ()
main = do
    tweet $ Tweet {text = "あかりからのテスト投稿"}
    -- autoTweet Nothing
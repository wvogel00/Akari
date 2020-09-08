{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

main :: IO ()
main = autoTweetYml "settings/tweet.yml"
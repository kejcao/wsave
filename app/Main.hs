{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wreq
import Control.Lens

main :: IO ()
main = do
  r <- get "http://example.org"
  print $ r ^. responseBody

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Network.Wreq hiding (header)
import Options.Applicative

data Arguments = Arguments
  {url :: String}

parseArgs :: Parser Arguments
parseArgs =
  Arguments
    <$> strOption
      ( long "url"
          <> short 'u'
          <> metavar "URL"
          <> help "URL of website to save"
      )

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (parseArgs <**> helper)
        ( fullDesc
            <> progDesc "Save/mirror the URL website"
            <> header "mirror a website"
        )

run :: Arguments -> IO ()
run (Arguments url) = do
  r <- get url
  print $ r ^. responseBody

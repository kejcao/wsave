{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Network.Wreq hiding (header)
import Options.Applicative
import System.Directory
import qualified URL

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

save :: String -> IO ()
save url = do
  r <- get url
  print $ r ^. responseBody

run :: Arguments -> IO ()
run (Arguments url) = do
  createDirectory $ URL.host
    (case URL.parseURL url of
      Right v -> v
      Left e -> error (show e))
  save url

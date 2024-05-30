{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Network.Wreq hiding (header)
import Options.Applicative
import System.Directory
-- import System.IO
import qualified Data.ByteString.Lazy as B
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
  B.writeFile "index.html" (r ^. responseBody)

run :: Arguments -> IO ()
run (Arguments s) = do
  let host = URL.host url
  createDirectory host
  setCurrentDirectory host
  save (URL.toString url)
  where
    url =
      case URL.parseURL s of
        Right v -> v
        Left e -> error (show e)

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Network.Wreq hiding (header)
import Options.Applicative
import System.Directory
import qualified Data.ByteString.Lazy as B
import qualified Url

data Arguments = Arguments
  {url :: String}

parseArgs :: Parser Arguments
parseArgs =
  Arguments
    <$> strOption
      ( long "url"
          <> short 'u'
          <> metavar "Url"
          <> help "Url of website to save"
      )

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (parseArgs <**> helper)
        ( fullDesc
            <> progDesc "Save/mirror the Url website"
            <> header "mirror a website"
        )

-- saveAssets :: String -> IO ()
-- saveAssets src = 

save :: String -> IO ()
save url = do
  r <- get url
  B.writeFile "index.html" (r ^. responseBody)

run :: Arguments -> IO ()
run (Arguments s) = do
  let host = Url.host url
  createDirectory host
  setCurrentDirectory host
  save (Url.toString url)
  where
    url =
      case Url.parseUrl s of
        Right v -> v
        Left e -> error (show e)

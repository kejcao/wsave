{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Network.Wreq hiding (header)
import Options.Applicative
import System.Directory
import System.FilePath.Posix (takeDirectory)
import qualified Data.ByteString.Lazy as B
import qualified Url
import qualified Data.Text as T
import qualified Html
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TI

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

saveAsset oldUrl path = do
  putStrLn $ "saving asset " <> (Url.toString url)
  createDirectoryIfMissing True $ takeDirectory savePath
  r <- get $ Url.toString url
  B.writeFile savePath (r ^. responseBody)
  where
    url = Url.orient oldUrl path
    savePath = if head path == '/' then tail path else path

mapIf p f = map (\x -> if p x then f x else x)

saveAssets :: Url.Url -> T.Text -> IO T.Text
saveAssets url src = do
  let toks = Html.parseTokens src
  let assets = concat $ map ((\(Html.TagSelfClose _ a) -> map (\(Html.Attr _ v) -> v) $ filter isSrcAttribute a)) (filter isImg (map snd toks))
  let paths = filter (/="") $ filter (not . T.isPrefixOf "data:") assets
  -- print $ paths
  mapM_ (saveAsset url . T.unpack) paths
  let test = mapIf (isImg . snd) (\(_, (Html.TagSelfClose _ attrs)) -> let new = Html.TagSelfClose "img" (adjust attrs) in (Html.renderToken new, new)) toks
  print $ test
  return (Html.recoverTokens $ test)
  where
    isSrcAttribute (Html.Attr "src" value) = True
    isSrcAttribute (Html.Attr "data-src" value) = True
    isSrcAttribute _ = False
 
    adjust :: [Html.Attr] -> [Html.Attr]
    adjust = mapIf isSrcAttribute (\(Html.Attr k v) -> (Html.Attr k (if T.head v == '/' then (T.pack $ concat $ replicate (length $ filter (=='/') $ Url.path url) "../") <> T.tail v else v)))

    isImg (Html.TagSelfClose "img" _) = True
    isImg _ = False

save :: Url.Url -> IO ()
save url = do
  r <- get $ Url.toString url
  let body = r ^. responseBody
  newSrc <- saveAssets url (TE.decodeUtf8 $ B.toStrict body)
  if Url.path url == ""
    then TI.writeFile "index.html" newSrc
    else do
      let fp = Url.path url
      createDirectoryIfMissing True $ takeDirectory fp
      TI.writeFile (fp <> ".html") newSrc

run :: Arguments -> IO ()
run (Arguments urlString) = do
  let host = Url.host url
  createDirectory host
  setCurrentDirectory host
  save url
  where
    url =
      case Url.parseUrl urlString of
        Right v -> v
        Left e -> error (show e)

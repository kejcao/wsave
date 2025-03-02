{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Network.Wreq hiding (header)
import Options.Applicative
import System.Directory
import System.FilePath.Posix (takeDirectory)
import qualified Data.ByteString.Lazy as B
import qualified Url
import Data.Text (Text)
import qualified Data.Text as T
import qualified Html
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TI
import Data.Char (isAlphaNum, toUpper)
import Numeric (showHex)

data Arguments = Arguments
  {saveUrl :: String}

parseArgs :: Parser Arguments
parseArgs =
  Arguments
    <$> strOption
      ( long "saveUrl"
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
  putStrLn $ "saving asset " <> Url.toString url
  createDirectoryIfMissing True $ takeDirectory savePath
  r <- get $ Url.toString url
  B.writeFile savePath (r ^. responseBody)
  where
    url = Url.orient oldUrl path
    savePath = if head path == '/' then tail path else path

attrValue :: Html.Attr -> Text
attrValue (Html.Attr _ v) = v

isValidUrl :: Text -> Bool
isValidUrl url =
  case Url.parseUrl (T.unpack url) of
    Right _ -> True
    Left  _ -> False

isSrcAttribute :: Html.Attr -> Bool
isSrcAttribute (Html.Attr "src" _) = True
isSrcAttribute (Html.Attr "data-src" _) = True
isSrcAttribute _ = False

isHrefAttribute :: Html.Attr -> Bool
isHrefAttribute (Html.Attr "href" _) = True
isHrefAttribute _ = False

percentEncode :: Text -> Text
percentEncode =
  T.concatMap
    (\c ->
      if isAlphaNum c || c == '/' -- DO NOT percent encode "/" because that's used in file path
        then T.pack [c]
        else T.pack $ '%' : (toUpper <$> showHex (fromEnum c) "")
    )

saveAssets :: Url.Url -> Text -> IO Text
saveAssets url src = do
  let toks = Html.parseTokens src
  let assets' = concatMap (filter (/="") . filter (not . T.isPrefixOf "data:") . match . snd) toks
  let assets = filter (not . isValidUrl) assets' -- TODO add support to download external URLs
  mapM_ (saveAsset url . T.unpack) assets

  let newSrc = map adjust toks
  return (Html.recoverTokens newSrc)
  where
    match (Html.TagSelfClose "img" a) = map attrValue $ filter isSrcAttribute a
    match (Html.TagOpen "img" a) = map attrValue $ filter isSrcAttribute a
    match (Html.TagSelfClose "link" a) = map attrValue $ filter isHrefAttribute a
    match (Html.TagOpen "link" a) = map attrValue $ filter isHrefAttribute a
    match _ = []

    g "" = ""
    g p = if T.head p == '/' then absolute p else p
      where absolute path = T.pack (concat $ replicate (length $ filter (=='/') $ Url.path url) "../") <> T.tail path

    -- For some reason this works. Please help.
    adjustAttrs :: (Html.Attr -> Bool) -> [Html.Attr] -> [Html.Attr]
    adjustAttrs p = mapIf p (\(Html.Attr k v) -> Html.Attr k (percentEncode v))
      where mapIf p f = map (\x -> if p x then f x else x)

    adjust (_, Html.TagSelfClose "img" attrs) =
      let new = Html.TagSelfClose "img" (adjustAttrs isSrcAttribute attrs)
      in (Html.renderToken new, new)
    adjust (_, Html.TagOpen "link" attrs) =
      let new = Html.TagOpen "link" (adjustAttrs isHrefAttribute attrs)
      in (Html.renderToken new, new)
    adjust x = x

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

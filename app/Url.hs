{-# LANGUAGE OverloadedStrings #-}

module Url where

import Text.Parsec
import Text.Parsec.String (Parser)

data Url = Url
  { scheme   :: String
  , host     :: String
  , port     :: Maybe Int
  , path     :: String
  -- , query    :: Maybe String
  -- , fragment :: Maybe String
  }
  deriving (Show)

orient :: Url -> String -> Url
orient url newPath
  | head newPath == '/' = url {path = tail newPath}
  | otherwise = url {path = path url ++ (if path url == "" || last (path url) == '/' then "" else "/") ++ newPath}

toString :: Url -> String
toString (Url scheme host port path) =
  scheme
    ++ "://"
    ++ host
    ++ maybe "" ((":" ++) . show) port
    ++ "/"
    ++ path

urlParser :: Parser Url
urlParser =
  Url
    <$> ((try (string "https") <|> string "http") <* string "://")
    <*> many1 (alphaNum <|> oneOf ".-")
    <*> optionally (read <$> (char ':' *> many1 digit))
    <*> ((char '/' *> many (noneOf "#")) <|> string "")
  where
    optionally p = Just <$> p <|> return Nothing

parseUrl :: String -> Either ParseError Url
parseUrl = parse urlParser ""

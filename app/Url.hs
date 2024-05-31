{-# LANGUAGE OverloadedStrings #-}

module Url where

import Text.Parsec
import Text.Parsec.String (Parser)

data Url = Url
  { scheme   :: String
  , host     :: String
  , port     :: Maybe Int
  , path     :: String
  , query    :: Maybe String
  , fragment :: Maybe String
  }
  deriving (Show)

toString :: Url -> String
toString (Url scheme host port path query _) =
  scheme
    ++ "://"
    ++ host
    ++ maybe "" ((":" ++) . show) port
    ++ "/"
    ++ path
    ++ maybe "" ("?" ++) query

urlParser :: Parser Url
urlParser =
  Url
    <$> ((try (string "https") <|> string "http") <* string "://")
    <*> many1 (alphaNum <|> oneOf ".-")
    <*> optionally (read <$> (char ':' *> many1 digit))
    <*> ((char '/' *> many (noneOf "?#")) <|> string "")
    <*> optionally (char '?' *> many (noneOf "#"))
    <*> optionally (char '#' *> many anyChar)
  where
    optionally p = Just <$> p <|> return Nothing

parseUrl :: String -> Either ParseError Url
parseUrl = parse urlParser ""

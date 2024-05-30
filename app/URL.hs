{-# LANGUAGE OverloadedStrings #-}

module URL where

import Text.Parsec
import Text.Parsec.String (Parser)

data URL = URL
  { scheme   :: String
  , host     :: String
  , port     :: Maybe Int
  , path     :: String
  , query    :: Maybe String
  , fragment :: Maybe String
  }
  deriving (Show)

-- toString :: URL -> String
-- toString (URL scheme host port path query fragment) =
--   scheme
--     ++ "://"
--     ++ host
--     ++ maybe "" ((":" ++) . show) port
--     ++ "/"
--     ++ path
--     ++ maybe "" ("?" ++) query
--     ++ maybe "" ("#" ++) fragment

toString :: URL -> String
toString (URL scheme host port path query _) =
  scheme
    ++ "://"
    ++ host
    ++ maybe "" ((":" ++) . show) port
    ++ "/"
    ++ path
    ++ maybe "" ("?" ++) query

urlParser :: Parser URL
urlParser =
  URL
    <$> ((try (string "https") <|> string "http") <* string "://")
    <*> many1 (alphaNum <|> oneOf ".-")
    <*> optionally (read <$> (char ':' *> many1 digit))
    <*> ((char '/' *> many (noneOf "?#")) <|> string "")
    <*> optionally (char '?' *> many (noneOf "#"))
    <*> optionally (char '#' *> many anyChar)
  where
    optionally p = Just <$> p <|> return Nothing

parseURL :: String -> Either ParseError URL
parseURL = parse urlParser ""

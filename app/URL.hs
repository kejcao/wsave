{-# LANGUAGE OverloadedStrings #-}

module URL where

import Text.Parsec
import Text.Parsec.String (Parser)

data URL = URL
  { host     :: String
  , port     :: Maybe Int
  , path     :: String
  -- , query    :: String
  -- , fragment :: String
  } deriving (Show)

urlParser :: Parser URL
urlParser =
  (try (string "http://") <|> string "https://") *>
    (URL
      <$> many1 (alphaNum <|> oneOf ".-")
      <*> (Just . read <$> (char ':' *> many1 digit) <|> return Nothing)
      <*> (char '/' *> many (noneOf "?#")))
      -- <*> try (char '?' *> many (noneOf "#"))
      -- <*> try (char '#' *> many anyChar)))

parseURL :: String -> Either ParseError URL
parseURL = parse urlParser ""

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -O2 #-}

-- Thanks https://github.com/bgamari/html-parse

module Html
  ( parseTokens,
    recoverTokens,
    renderToken,
    token,
    Token (..),
    Attr (..),
    TagName,
    AttrName,
    AttrValue,
  ) where

import Control.Applicative
import Control.Monad (guard)
import Data.Attoparsec.Text
import Data.Char hiding (isSpace)
import Data.List (unfoldr)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import GHC.Generics
import Prelude hiding (take, takeWhile)

-- import Text.HTML.Parser.Entities (entities)
-- import qualified Data.Trie as Trie

-- Section numbers refer to W3C HTML 5.2 specification.

type TagName   = Text
type AttrName  = Text
type AttrValue = Text

data Token
  = TagOpen !TagName [Attr]
  | TagSelfClose !TagName [Attr]
  | TagClose !TagName
  | ContentText !Text
  | ContentChar !Char
  | Comment !Builder
  | Doctype !Text
  deriving (Show, Ord, Eq, Generic)

data Attr = Attr !AttrName !AttrValue
  deriving (Show, Eq, Ord)

-- | This is a bit of a hack
endOfFileToken :: Token
endOfFileToken = ContentText ""

-- | Parse a single 'Token'.
token :: Parser Token
token = dataState -- Start in the data state.

-- | /§8.2.4.1/: Data state
dataState :: Parser Token
dataState = do
  content <- takeWhile (\c -> c /= '<' && c /= '&')
  if not $ T.null content
    then return $ ContentText content
    else
      choice
        [ char '<' >> tagOpen,
          -- , try $ char '&' >> charRef
          ContentChar '&' <$ char '&'
        ]

-- TODO add support for entities
-- charRef :: Parser Token
-- charRef = go entityTrie
--   where
--     go :: Trie.Trie Char Token -> Parser Token
--     go trie = do
--       c <- anyChar
--       case c of
--         ';' -> maybe empty return (Trie.terminal trie)
--         _ -> go (Trie.step c trie)

-- entityTrie :: Trie.Trie Char Token
-- entityTrie = Trie.fromList
--     [ (T.unpack name, ContentText expansion)
--     | (name, expansion) <- entities
--     ]

-- | /§8.2.4.6/: Tag open state
tagOpen :: Parser Token
tagOpen =
  (char '!' >> markupDeclOpen)
    <|> (char '/' >> endTagOpen)
    <|> (char '?' >> bogusComment mempty)
    <|> tagNameOpen
    <|> other
  where
    other = do
      return $ ContentChar '<'

-- | /§8.2.4.7/: End tag open state
endTagOpen :: Parser Token
endTagOpen = tagNameClose

-- | Equivalent to @inClass "\x09\x0a\x0c "@
isWhitespace :: Char -> Bool
isWhitespace '\x09' = True
isWhitespace '\x0a' = True
isWhitespace '\x0c' = True
isWhitespace '\x0d' = True
isWhitespace ' ' = True
isWhitespace _ = False

orC :: (Char -> Bool) -> (Char -> Bool) -> Char -> Bool
orC f g c = f c || g c
{-# INLINE orC #-}

isC :: Char -> Char -> Bool
isC = (==)
{-# INLINE isC #-}

-- | /§8.2.4.8/: Tag name state: the open case
--
-- deviation: no lower-casing, don't handle NULL characters
tagNameOpen :: Parser Token
tagNameOpen = do
  tag <- tagName'
  (satisfy isWhitespace >> beforeAttrName tag [])
    <|> (char '/' >> selfClosingStartTag tag [])
    <|> (char '>' >> return (TagOpen tag []))

-- | /§8.2.4.10/: Tag name state: close case
tagNameClose :: Parser Token
tagNameClose = do
  tag <- tagName'
  char '>' >> return (TagClose tag)

-- | /§8.2.4.10/: Tag name state: common code
--
-- deviation: no lower-casing, don't handle NULL characters
tagName' :: Parser Text
tagName' = do
  c <- peekChar'
  guard $ isAsciiUpper c || isAsciiLower c
  takeWhile $ not . (isWhitespace `orC` isC '/' `orC` isC '<' `orC` isC '>')

-- | /§8.2.4.40/: Self-closing start tag state
selfClosingStartTag :: TagName -> [Attr] -> Parser Token
selfClosingStartTag tag attrs =
  do
    char '>' >> return (TagSelfClose tag attrs)
    <|> (endOfInput >> return endOfFileToken)
    <|> beforeAttrName tag attrs

-- | /§8.2.4.32/: Before attribute name state
--
-- deviation: no lower-casing
beforeAttrName :: TagName -> [Attr] -> Parser Token
beforeAttrName tag attrs = do
  skipWhile isWhitespace
  (char '/' >> selfClosingStartTag tag attrs)
    <|> (char '>' >> return (TagOpen tag attrs))
    -- <|> (char '\x00' >> attrName tag attrs) -- TODO: NULL
    <|> attrName tag attrs

-- | /§8.2.4.33/: Attribute name state
attrName :: TagName -> [Attr] -> Parser Token
attrName tag attrs = do
  name <- takeWhile $ not . (isWhitespace `orC` isC '/' `orC` isC '=' `orC` isC '>')
  (endOfInput >> afterAttrName tag attrs name)
    <|> (char '=' >> beforeAttrValue tag attrs name)
    <|> try
      ( do
          mc <- peekChar
          case mc of
            Just c | notNameChar c -> afterAttrName tag attrs name
            _ -> empty
      )
  where
    -- <|> -- TODO: NULL
    notNameChar = isWhitespace `orC` isC '/' `orC` isC '>'

-- | /§8.2.4.34/: After attribute name state
afterAttrName :: TagName -> [Attr] -> AttrName -> Parser Token
afterAttrName tag attrs name = do
  skipWhile isWhitespace
  (char '/' >> selfClosingStartTag tag attrs)
    <|> (char '=' >> beforeAttrValue tag attrs name)
    <|> (char '>' >> return (TagOpen tag (Attr name T.empty : attrs)))
    <|> (endOfInput >> return endOfFileToken)
    <|> attrName tag (Attr name T.empty : attrs) -- not exactly sure this is right

-- | /§8.2.4.35/: Before attribute value state
beforeAttrValue :: TagName -> [Attr] -> AttrName -> Parser Token
beforeAttrValue tag attrs name = do
  skipWhile isWhitespace
  (char '"' >> attrValueDQuoted tag attrs name)
    <|> (char '\'' >> attrValueSQuoted tag attrs name)
    <|> (char '>' >> return (TagOpen tag (Attr name T.empty : attrs)))
    <|> attrValueUnquoted tag attrs name

-- | /§8.2.4.36/: Attribute value (double-quoted) state
attrValueDQuoted :: TagName -> [Attr] -> AttrName -> Parser Token
attrValueDQuoted tag attrs name = do
  value <- takeWhile (/= '"')
  _ <- char '"'
  afterAttrValueQuoted tag attrs name value

-- | /§8.2.4.37/: Attribute value (single-quoted) state
attrValueSQuoted :: TagName -> [Attr] -> AttrName -> Parser Token
attrValueSQuoted tag attrs name = do
  value <- takeWhile (/= '\'')
  _ <- char '\''
  afterAttrValueQuoted tag attrs name value

-- | /§8.2.4.38/: Attribute value (unquoted) state
attrValueUnquoted :: TagName -> [Attr] -> AttrName -> Parser Token
attrValueUnquoted tag attrs name = do
  value <- takeTill $ isWhitespace `orC` isC '>'
  (satisfy isWhitespace >> beforeAttrName tag attrs) -- unsure: don't emit?
    <|> (char '>' >> return (TagOpen tag (Attr name value : attrs)))
    <|> (endOfInput >> return endOfFileToken)

-- | /§8.2.4.39/: After attribute value (quoted) state
afterAttrValueQuoted :: TagName -> [Attr] -> AttrName -> AttrValue -> Parser Token
afterAttrValueQuoted tag attrs name value =
  (satisfy isWhitespace >> beforeAttrName tag attrs')
    <|> (char '/' >> selfClosingStartTag tag attrs')
    <|> (char '>' >> return (TagOpen tag attrs'))
    <|> (endOfInput >> return endOfFileToken)
  where
    attrs' = Attr name value : attrs

-- | /§8.2.4.41/: Bogus comment state
bogusComment :: Builder -> Parser Token
bogusComment content =
  do
    char '>' >> return (Comment content)
    <|> (endOfInput >> return (Comment content))
    <|> (char '\x00' >> bogusComment (content <> "\xfffd"))
    <|> (anyChar >>= \c -> bogusComment (content <> B.singleton c))

-- | /§8.2.4.42/: Markup declaration open state
markupDeclOpen :: Parser Token
markupDeclOpen =
  try comment_
    <|> try docType
    <|> bogusComment mempty
  where
    comment_ = char '-' >> char '-' >> commentStart
    docType = do
      -- switching this to asciiCI slowed things down by a factor of two
      s <- take 7
      guard $ T.toLower s == "doctype"
      doctype

-- | /§8.2.4.43/: Comment start state
commentStart :: Parser Token
commentStart =
  do
    char '-' >> commentStartDash
    <|> (char '>' >> return (Comment mempty))
    <|> comment mempty

-- | /§8.2.4.44/: Comment start dash state
commentStartDash :: Parser Token
commentStartDash =
  (char '-' >> commentEnd mempty)
    <|> (char '>' >> return (Comment mempty))
    <|> (endOfInput >> return (Comment mempty))
    <|> comment (B.singleton '-')

-- | /§8.2.4.45/: Comment state
comment :: Builder -> Parser Token
comment content0 = do
  content <- B.fromText <$> takeWhile (not . (isC '-' `orC` isC '\x00' `orC` isC '<'))
  (char '<' >> commentLessThan (content0 <> content <> "<"))
    <|> (char '-' >> commentEndDash (content0 <> content))
    <|> (char '\x00' >> comment (content0 <> content <> B.singleton '\xfffd'))
    <|> (endOfInput >> return (Comment $ content0 <> content))

-- | /§8.2.46/: Comment less-than sign state
commentLessThan :: Builder -> Parser Token
commentLessThan content =
  (char '!' >> commentLessThanBang (content <> "!"))
    <|> (char '<' >> commentLessThan (content <> "<"))
    <|> comment content

-- | /§8.2.47/: Comment less-than sign bang state
commentLessThanBang :: Builder -> Parser Token
commentLessThanBang content =
  (char '-' >> commentLessThanBangDash content)
    <|> comment content

-- | /§8.2.48/: Comment less-than sign bang dash state
commentLessThanBangDash :: Builder -> Parser Token
commentLessThanBangDash content =
  (char '-' >> commentLessThanBangDashDash content)
    <|> commentEndDash content

-- | /§8.2.49/: Comment less-than sign bang dash dash state
commentLessThanBangDashDash :: Builder -> Parser Token
commentLessThanBangDashDash content =
  (char '>' >> comment content)
    <|> (endOfInput >> comment content)
    <|> commentEnd content

-- | /§8.2.4.50/: Comment end dash state
commentEndDash :: Builder -> Parser Token
commentEndDash content =
  do
    char '-' >> commentEnd content
    <|> (endOfInput >> return (Comment content))
    <|> comment (content <> "-")

-- | /§8.2.4.51/: Comment end state
commentEnd :: Builder -> Parser Token
commentEnd content =
  do
    char '>' >> return (Comment content)
    <|> (char '!' >> commentEndBang content)
    <|> (char '-' >> commentEnd (content <> "-"))
    <|> (endOfInput >> return (Comment content))
    <|> comment (content <> "--")

-- | /§8.2.4.52/: Comment end bang state
commentEndBang :: Builder -> Parser Token
commentEndBang content =
  do
    char '-' >> commentEndDash (content <> "--!")
    <|> (char '>' >> return (Comment content))
    <|> (endOfInput >> return (Comment content))
    <|> comment (content <> "--!")

-- | /§8.2.4.53/: DOCTYPE state
-- FIXME
doctype :: Parser Token
doctype = do
  content <- takeTill (== '>')
  _ <- char '>'
  return $ Doctype content

parseTokens :: Text -> [(Text, Token)]
parseTokens = unfoldr f
  where
    f :: Text -> Maybe ((Text, Token), Text)
    f t
      | T.null t = Nothing
      | otherwise =
          case parse (match token) t of
            Done rest tok -> Just (tok, rest)
            Partial cont ->
              case cont mempty of
                Done rest tok -> Just (tok, rest)
                _ -> Nothing

recoverTokens :: [(Text, Token)] -> Text
recoverTokens l = foldl (<>) "" (map fst l)

renderToken :: Token -> T.Text
renderToken =
  mconcat . \case
    (TagOpen n []) -> ["<", n, ">"]
    (TagOpen n attrs) -> ["<", n, " ", renderAttrs attrs, ">"]
    (TagSelfClose n attrs) -> ["<", n, " ", renderAttrs attrs, " />"]
    (TagClose n) -> ["</", n, ">"]
    (ContentChar c) -> [T.singleton c]
    (ContentText t) -> [t]
    (Comment builder) -> ["<!--", TL.toStrict $ B.toLazyText builder, "-->"]
    (Doctype t) -> ["<!DOCTYPE", t, ">"]

-- | See 'renderAttr'.
renderAttrs :: [Attr] -> Text
renderAttrs = T.unwords . fmap renderAttr . reverse

-- | Does not escape quotation in attribute values!
renderAttr :: Attr -> Text
renderAttr (Attr k v) = mconcat [k, "=\"", v, "\""]

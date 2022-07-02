module Silene.Parser (parseText) where

import Control.Applicative (empty, some, (<|>))
import qualified Control.Monad as Monad
import qualified Data.Char as Char
import Data.Text (Text)
import Data.Void (Void)
import Silene.RawSyntax (Name, Raw (..))
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Megaparsec.Parsec Void Text

-- | ws for is for lexing whitespace (including single and multiline comments).
ws :: Parser ()
ws = L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

-- | withPos adds source position information to an existing parser.
--   This is useful for reporting errors.
withPos :: Parser Raw -> Parser Raw
withPos p = RSrcPos <$> Megaparsec.getSourcePos <*> p

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

-- | Symbol lexes a `symbol`. A `symbol` can be any sequence of characters excluding keywords.
--   Usually this means variables.
symbol :: Text -> Parser Text
symbol = lexeme . C.string

-- | Lex a single character.
char :: Char -> Parser Char
char = lexeme . C.char

parens :: Parser a -> Parser a
parens p = char '(' *> p <* char ')'

-- | Reserved keywords
keyword :: Text -> Bool
keyword x = x == "let" || x == "in" || x == "λ" || x == "U"

pArrow :: Parser Text
pArrow = symbol "→" <|> symbol "->"

pIdent :: Parser Name
pIdent = Megaparsec.try $ do
  x <- Megaparsec.takeWhile1P Nothing Char.isAlphaNum
  Monad.guard (not (keyword x))
  x <$ ws

pKeyword :: Text -> Parser ()
pKeyword kw = do
  _ <- C.string kw
  (Megaparsec.takeWhile1P Nothing Char.isAlphaNum *> empty) <|> ws

pAtom :: Parser Raw
pAtom =
  withPos ((RVar <$> pIdent) <|> (RU <$ symbol "U"))
    <|> parens pRaw

pBinder :: Parser Text
pBinder = pIdent <|> symbol "_"

pSpine :: Parser Raw
pSpine = foldl1 RApp <$> some pAtom

pLam :: Parser Raw
pLam = do
  _ <- char 'λ' <|> char '\\'
  xs <- some pBinder
  _ <- char '.'
  t <- pRaw
  pure (foldr RLam t xs)

pPi :: Parser Raw
pPi = do
  dom <- some (parens ((,) <$> some pBinder <*> (char ':' *> pRaw)))
  _ <- pArrow
  cod <- pRaw
  pure $ foldr (\(xs, a) t -> foldr (`RPi` a) t xs) cod dom

funOrSpine :: Parser Raw
funOrSpine = do
  sp <- pSpine
  Megaparsec.optional pArrow >>= \case
    Nothing -> pure sp
    Just _ -> RPi "_" sp <$> pRaw

pLet :: Parser Raw
pLet = do
  _ <- pKeyword "let"
  x <- pBinder
  _ <- symbol ":"
  a <- pRaw
  _ <- symbol "="
  t <- pRaw
  _ <- pKeyword "in"
  RLet x a t <$> pRaw

pRaw :: Parser Raw
pRaw = withPos (pLam <|> pLet <|> Megaparsec.try pPi <|> funOrSpine)

pSrc :: Parser Raw
pSrc = ws *> pRaw <* Megaparsec.eof

parseText :: Text -> Either String Raw
parseText src =
  case Megaparsec.parse pSrc "(source)" src of
    Left e -> Left $ Megaparsec.errorBundlePretty e
    Right t -> pure t

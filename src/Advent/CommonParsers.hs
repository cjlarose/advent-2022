module Advent.CommonParsers
  ( linesOf
  , natural
  , integerWithOptionalLeadingSign
  , token
  , symbol
  ) where

import Numeric.Natural (Natural)
import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec (Parsec, some, sepEndBy1, eof, option, (<|>))
import Text.Megaparsec.Char (newline, digitChar, char, string, space)

type Parser = Parsec Void Text

natural :: Parser Natural
natural = read <$> some digitChar

integerWithOptionalLeadingSign :: Integral a => Parser a
integerWithOptionalLeadingSign = (*) <$> option 1 sign <*> (fromIntegral <$> natural)
  where
    sign = (-1 <$ char '-') <|> (1 <$ char '+')

linesOf :: Parser a -> Parser [a]
linesOf p = sepEndBy1 p newline <* eof

token :: Parser a -> Parser a
token p = p <* space

symbol :: Text -> Parser Text
symbol = token . string

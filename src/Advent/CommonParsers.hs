module Advent.CommonParsers
  ( linesOf,
    natural,
    integerWithOptionalLeadingSign,
    unsignedBinaryInteger,
    token,
    symbol,
  )
where

import Advent.BitUtils (fromBits)
import Data.Bits (Bits)
import Data.Text (Text)
import Data.Void (Void)
import Numeric.Natural (Natural)
import Text.Megaparsec (Parsec, eof, option, sepEndBy1, some, (<|>))
import Text.Megaparsec.Char (char, digitChar, newline, space, string)

type Parser = Parsec Void Text

natural :: Parser Natural
natural = read <$> some digitChar

integerWithOptionalLeadingSign :: Integral a => Parser a
integerWithOptionalLeadingSign = (*) <$> option 1 sign <*> (fromIntegral <$> natural)
  where
    sign = (-1 <$ char '-') <|> (1 <$ char '+')

unsignedBinaryInteger :: Bits a => Parser a
unsignedBinaryInteger = fromBits <$> some bit
  where
    bit :: Parser Bool
    bit = False <$ char '0' <|> True <$ char '1'

linesOf :: Parser a -> Parser [a]
linesOf p = sepEndBy1 p newline <* eof

token :: Parser a -> Parser a
token p = p <* space

symbol :: Text -> Parser Text
symbol = token . string

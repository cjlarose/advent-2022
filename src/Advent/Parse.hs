{-# LANGUAGE OverloadedStrings #-}

module Advent.Parse
  ( Parser,
    parse,
  )
where

import Advent.PuzzleAnswerPair (PuzzleAnswerPair)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty)
import qualified Text.Megaparsec as Megaparsec

type Parser = Parsec Void Text

parse :: Parser a -> (a -> PuzzleAnswerPair) -> Text -> Either String PuzzleAnswerPair
parse p f x = either (Left . errorBundlePretty) (Right . f) $ Megaparsec.parse p "" x

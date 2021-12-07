module Advent.Input
  ( getProblemInputAsText,
    withSuccessfulParse,
  )
where

import Advent.Parse (Parser)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair)
import Data.Text (Text)
import qualified Data.Text.IO (readFile)
import Text.Megaparsec (parse)

path :: Int -> String
path problemNumber = "inputs/" ++ show problemNumber ++ ".txt"

getProblemInputAsText :: Int -> IO Text
getProblemInputAsText problemNumber = do
  let filePath = path problemNumber
  Data.Text.IO.readFile filePath

withSuccessfulParse :: Parser a -> (a -> PuzzleAnswerPair) -> Text -> Either String PuzzleAnswerPair
withSuccessfulParse p f x = either (Left . show) (Right . f) $ parse p "" x

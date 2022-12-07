{-# LANGUAGE OverloadedStrings #-}
module Advent2022.Day05
  ( solve,
  )
where

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair (..))
import Advent.CommonParsers (token, symbol, natural)
import Data.List (transpose)
import qualified Data.Vector as V
import Numeric.Natural (Natural)
import Control.Monad.State (State, evalState, get, put)
import Text.Megaparsec (some, sepBy1, (<|>), count, try, eof)
import Text.Megaparsec.Char (char, digitChar, upperChar, eol)

type Stack = [Char]
type Stacks = V.Vector Stack
data StackMove = StackMove
  { quantity :: Natural
  , source :: Natural
  , destination :: Natural
  }
data CrateSpot = Crate Char | EmptySpace
data ProblemInput = ProblemInput
  { inputStacks :: Stacks
  , inputMoves :: [StackMove]
  }

inputParser :: Parser ProblemInput
inputParser = ProblemInput <$> (stacks <* eol) <*> some stackMove <* eof

stacks :: Parser Stacks
stacks = makeStacks <$> some (stackLine <* eol) <*> (stackLabels <* eol)
  where
    stackLine = sepBy1 (crate <|> emptySpace) (char ' ')
    crate = Crate <$> (char '[' *> upperChar) <* char ']'
    emptySpace = EmptySpace <$ (try $ count 3 (char ' '))
    stackLabels = length <$> sepBy1 (char ' ' *> digitChar <* char ' ') (char ' ')
    unboxSpaces [] = []
    unboxSpaces (EmptySpace : xs) = unboxSpaces xs
    unboxSpaces (Crate c : xs) = c : unboxSpaces xs
    makeStacks spots n = V.fromListN n . map unboxSpaces . transpose $ spots

stackMove :: Parser StackMove
stackMove = makeMove <$> quantityP <*> sourceP <*> destP
  where
    quantityP = symbol "move" *> token natural
    sourceP = symbol "from" *> token natural
    destP = symbol "to" *> token natural
    makeMove n i j = StackMove n (i - 1) (j - 1)

applyStackMoves :: Bool -> [StackMove] -> State Stacks ()
applyStackMoves moveMultipleCrates = mapM_ applyStackMove
  where
    transformCrates = if moveMultipleCrates then id else reverse
    applyStackMove :: StackMove -> State Stacks ()
    applyStackMove move = do
      stacks <- get
      let i = fromIntegral . source $ move
      let j = fromIntegral . destination $ move
      let n = fromIntegral . quantity $ move
      let sourceStack = stacks V.! i
      let destStack = stacks V.! j
      let (crates, newSource) = splitAt n sourceStack
      let newDest = transformCrates crates ++ destStack
      let newStacks = stacks V.// [(i, newSource), (j, newDest)]
      put newStacks

getTopOfStacks :: State Stacks [Char]
getTopOfStacks = map head . V.toList <$> get

executeStackMoves :: Bool -> ProblemInput -> String
executeStackMoves moveMultipleCrates input = evalState (applyStackMoves moveMultipleCrates (inputMoves input) >> getTopOfStacks) . inputStacks $ input

printResults :: ProblemInput -> PuzzleAnswerPair
printResults input = PuzzleAnswerPair (part1, part2)
  where
    part1 = executeStackMoves False input
    part2 = executeStackMoves True input

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 5

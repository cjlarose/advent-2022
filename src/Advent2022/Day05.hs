{-# LANGUAGE OverloadedStrings #-}
module Advent2022.Day05
  ( solve,
  )
where

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair (..))
import Advent.CommonParsers (linesOf, token, symbol, natural)
import Data.List (sort, intersect, transpose)
import Data.Char (isLower, ord)
import qualified Data.Vector as V
import Numeric.Natural (Natural)
import Control.Monad.State (State, evalState, get, put)
import Text.Megaparsec (some, sepBy1, (<|>), count, try, eof)
import Text.Megaparsec.Char (char, digitChar, upperChar, spaceChar, eol)

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
    stackLine :: Parser [CrateSpot]
    stackLine = sepBy1 (crate <|> emptySpace) (char ' ')

    crate :: Parser CrateSpot
    crate = Crate <$> (char '[' *> upperChar) <* char ']'

    emptySpace :: Parser CrateSpot
    emptySpace = EmptySpace <$ (try $ count 3 (char ' '))

    stackLabels :: Parser Int
    stackLabels = length <$> sepBy1 (char ' ' *> digitChar <* char ' ') (char ' ')

    unboxSpaces :: [CrateSpot] -> [Char]
    unboxSpaces [] = []
    unboxSpaces (EmptySpace : xs) = unboxSpaces xs
    unboxSpaces (Crate c : xs) = c : unboxSpaces xs

    makeStacks :: [[CrateSpot]] -> Int -> Stacks
    makeStacks spots n = V.fromListN n . map unboxSpaces . transpose $ spots

stackMove :: Parser StackMove
stackMove = makeMove <$> (symbol "move" *> token natural <* symbol "from") <*> (token natural <* symbol "to") <*> token natural
  where
    makeMove n i j = StackMove n (i - 1) (j - 1)

applyStackMoves :: [StackMove] -> State Stacks ()
applyStackMoves = mapM_ applyStackMove
  where
    applyStackMove :: StackMove -> State Stacks ()
    applyStackMove move = do
      stacks <- get
      let i = fromIntegral . source $ move
      let j = fromIntegral . destination $ move
      let n = fromIntegral . quantity $ move
      let sourceStack = stacks V.! i
      let destStack = stacks V.! j
      let (crates, newSource) = splitAt n sourceStack
      let newDest = crates ++ destStack
      let updates = [(i, newSource), (j, newDest)]
      let newStacks = stacks V.// updates
      put newStacks

getTopOfStacks :: State Stacks [Char]
getTopOfStacks = map head . V.toList <$> get

topOfStacks :: ProblemInput -> String
topOfStacks input = evalState (applyStackMoves (inputMoves input) >> getTopOfStacks) . inputStacks $ input

printResults :: ProblemInput -> PuzzleAnswerPair
printResults input = PuzzleAnswerPair (part1, part2)
  where
    part1 = topOfStacks input
    part2 = ""

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 5

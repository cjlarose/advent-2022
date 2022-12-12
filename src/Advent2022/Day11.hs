{-# LANGUAGE OverloadedStrings #-}
module Advent2022.Day11
  ( solve,
  )
where

import Data.List (sort)
import qualified Data.Map as Map
import Data.Map ((!))
import Text.Megaparsec (some, eof, (<|>), sepBy1)
import Text.Megaparsec.Char (char)
import Control.Monad (sequence_)
import Control.Monad.State.Lazy (State, execState, get, put)
import Control.Monad.Loops (whileM_)

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair (..))
import Advent.CommonParsers (integerWithOptionalLeadingSign, token, symbol)

data Monkey = Monkey
  { items :: [Int]
  , operation :: Int -> Int
  , recipientMonkey :: Int -> Int -- Item -> Monkey to throw to
  }
data Operand = Old | ImmediateConstant Int
data MonkeyGame = MonkeyGame
  { monkeys :: Map.Map Int Monkey
  , numInspections :: Map.Map Int Int -- Monkey ID -> number of inspections
  }

inputParser :: Parser [Monkey]
inputParser = some monkey <* eof
  where
    monkeyId = symbol "Monkey" *> token (integerWithOptionalLeadingSign <* char ':')
    startingItems = symbol "Starting items:" *> (token . sepBy1 integerWithOptionalLeadingSign . symbol $ ",")
    operand = (Old <$ symbol "old") <|> (ImmediateConstant <$> integerWithOptionalLeadingSign)
    makeOperation f Old x = f x x
    makeOperation f (ImmediateConstant imm) x = f x imm
    operation = makeOperation <$> (symbol "Operation: new = old" *> token (((*) <$ char '*') <|> (+) <$ char '+')) <*> token operand
    makeTest divisor mTrue mFalse x = if x `mod` divisor == 0 then mTrue else mFalse
    ifTrue = symbol "If true: throw to monkey" *> token integerWithOptionalLeadingSign
    ifFalse = symbol "If false: throw to monkey" *> token integerWithOptionalLeadingSign
    test :: Parser (Int -> Int)
    test = makeTest <$> (symbol "Test: divisible by" *> token integerWithOptionalLeadingSign) <*> ifTrue <*> ifFalse
    monkey = Monkey <$> (monkeyId *> startingItems) <*> operation <*> test

simulateRound :: State MonkeyGame ()
simulateRound = get >>= sequence_ . map simulateMonkey . Map.keys . monkeys
  where
    simulateMonkey :: Int -> State MonkeyGame ()
    simulateMonkey mid = whileM_ (monkeyHasItems mid) (inspectNextItem mid)

    getMonkey :: Int -> State MonkeyGame Monkey
    getMonkey mid = get >>= (\game -> return . (flip (!) mid) . monkeys $ game)

    monkeyHasItems :: Int -> State MonkeyGame Bool
    monkeyHasItems mid = getMonkey mid >>= return . not . null . items

    sendItemToMonkey :: Int -> Int -> State MonkeyGame ()
    sendItemToMonkey mid item = do
      game <- get
      let monkey = monkeys game ! mid
      let newItems = items monkey ++ [item]
      let newMonkey = monkey { items = newItems }
      let newMonkeys = Map.insert mid newMonkey . monkeys $ game
      put $ game { monkeys = newMonkeys }

    popItem :: Int -> State MonkeyGame Int
    popItem mid = do
      game <- get
      let monkey = monkeys game ! mid
      let (item : remaining) = items monkey
      let newMonkey = monkey { items = remaining }
      let newMonkeys = Map.insert mid newMonkey . monkeys $ game
      let newGame = game { monkeys = newMonkeys }
      put newGame
      return item

    recordInspectionForMonkey :: Int -> State MonkeyGame ()
    recordInspectionForMonkey mid = do
      game <- get
      let counts = numInspections game
      let currentCount = Map.findWithDefault 0 mid counts
      let newInspections = Map.insert mid (currentCount + 1) counts
      put $ game { numInspections = newInspections }

    inspectNextItem :: Int -> State MonkeyGame ()
    inspectNextItem mid = do
      item <- popItem mid
      monkey <- getMonkey mid
      let afterMonkeyOperation = operation monkey $ item
      recordInspectionForMonkey mid
      let newWorryLevel = afterMonkeyOperation `div` 3
      let recipient = recipientMonkey monkey $ newWorryLevel
      sendItemToMonkey recipient newWorryLevel

simulateRounds :: Int -> State MonkeyGame ()
simulateRounds n = sequence_ (take n . repeat $ simulateRound)

monkeyBusiness :: [Monkey] -> Int
monkeyBusiness monkeys = product . take 2 . topActivityLevels $ finalState
  where
    initialState = MonkeyGame (Map.fromList . zip [0..] $ monkeys) Map.empty
    finalState = execState (simulateRounds 20) initialState
    topActivityLevels = reverse . sort . Map.elems . numInspections

printResults :: [Monkey] -> PuzzleAnswerPair
printResults monkeys = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . monkeyBusiness $ monkeys
    part2 = "TODO"

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 11

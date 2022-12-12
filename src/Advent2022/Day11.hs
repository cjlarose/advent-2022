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
import Control.Monad.State.Lazy (State, execState, get, put, modify)
import Control.Monad.Loops (whileM_)

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair (..))
import Advent.CommonParsers (integerWithOptionalLeadingSign, token, symbol)

data Monkey = Monkey
  { items :: [Int]
  , operation :: Int -> Int
  , divisor ::  Int
  , recipientMonkey :: Int -> Int -- Item -> Monkey to throw to
  }
data Operand = Old | ImmediateConstant Int
data MonkeyGame = MonkeyGame
  { monkeys :: Map.Map Int Monkey
  , numInspections :: Map.Map Int Int -- Monkey ID -> number of inspections
  , gameDivisor :: Int
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
    makeTest divisor mTrue mFalse = (divisor, \x -> if x `mod` divisor == 0 then mTrue else mFalse)
    ifTrue = symbol "If true: throw to monkey" *> token integerWithOptionalLeadingSign
    ifFalse = symbol "If false: throw to monkey" *> token integerWithOptionalLeadingSign
    test :: Parser (Int, (Int -> Int))
    test = makeTest <$> (symbol "Test: divisible by" *> token integerWithOptionalLeadingSign) <*> ifTrue <*> ifFalse
    monkey = (\mid op (divisor, f) -> Monkey mid op divisor f) <$> (monkeyId *> startingItems) <*> operation <*> test

simulateRound :: (Int -> Int) -> State MonkeyGame ()
simulateRound updateWorryLevel = get >>= sequence_ . map simulateMonkey . Map.keys . monkeys
  where
    simulateMonkey :: Int -> State MonkeyGame ()
    simulateMonkey mid = whileM_ (monkeyHasItems mid) (inspectNextItem mid)

    lookupMonkey :: Int -> MonkeyGame -> Monkey
    lookupMonkey mid = (flip (!) mid) . monkeys

    monkeyHasItems :: Int -> State MonkeyGame Bool
    monkeyHasItems mid = not . null . items . lookupMonkey mid <$> get

    updateMonkey :: Int -> Monkey -> State MonkeyGame ()
    updateMonkey mid monkey =
      modify $ \game -> game { monkeys = Map.insert mid monkey . monkeys $ game }

    sendItemToMonkey :: Int -> Int -> State MonkeyGame ()
    sendItemToMonkey mid item = do
      game <- get
      let monkey = lookupMonkey mid game
      let newItems = items monkey ++ [item]
      let newMonkey = monkey { items = newItems }
      updateMonkey mid newMonkey

    popItem :: Int -> State MonkeyGame Int
    popItem mid = do
      game <- get
      let monkey = lookupMonkey mid game
      let (item : remaining) = items monkey
      let newMonkey = monkey { items = remaining }
      updateMonkey mid newMonkey
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
      game <- get
      let monkey = lookupMonkey mid game
      let afterMonkeyOperation = operation monkey item
      recordInspectionForMonkey mid
      let newWorryLevel = updateWorryLevel afterMonkeyOperation `mod` gameDivisor game
      let recipient = recipientMonkey monkey newWorryLevel
      sendItemToMonkey recipient newWorryLevel

simulateRounds :: Int -> (Int -> Int) -> State MonkeyGame ()
simulateRounds n updateWorryLevel = sequence_ (take n . repeat $ simulateRound updateWorryLevel)

monkeyBusiness :: Int -> (Int -> Int) -> [Monkey] -> Int
monkeyBusiness numRounds updateWorryLevel monkeys = product . take 2 . topActivityLevels $ finalState
  where
    d = product . map divisor $ monkeys -- LCM is probably better
    initialState = MonkeyGame (Map.fromList . zip [0..] $ monkeys) Map.empty d
    finalState = execState (simulateRounds numRounds updateWorryLevel) initialState
    topActivityLevels = reverse . sort . Map.elems . numInspections

printResults :: [Monkey] -> PuzzleAnswerPair
printResults monkeys = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . monkeyBusiness 20 (flip div 3) $ monkeys
    part2 = show . monkeyBusiness 10000 id $ monkeys

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 11

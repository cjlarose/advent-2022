module Advent2022.Day03
  ( solve,
  )
where

import Advent.CommonParsers (linesOf)
import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair (..))
import Data.List (sort, intersect)
import Data.Char (isLower, ord)
import Text.Megaparsec (some)
import Text.Megaparsec.Char (letterChar)

type ItemPriority = Int
type Rucksack = ([ItemPriority], [ItemPriority])

inputParser :: Parser [Rucksack]
inputParser = linesOf rucksack

rucksack :: Parser Rucksack
rucksack = readCompartments <$> some (priority <$> letterChar)
  where
    readCompartments xs = splitAt (length xs `div` 2) xs
    priority x | isLower x = ord x - ord 'a' + 1
               | otherwise = ord x - ord 'A' + 27

misplacedItemPrioritySum :: [Rucksack] -> Int
misplacedItemPrioritySum = sum . map f
  where
    f (left, right) = head $ left `intersect` right

groupPrioritySum :: [Rucksack] -> Int
groupPrioritySum [] = 0
groupPrioritySum (a : b : c : xs) = commonItem + groupPrioritySum xs
  where
    allItems (l, r) = l ++ r
    commonItem = head $ allItems a `intersect` allItems b `intersect` allItems c

printResults :: [Rucksack] -> PuzzleAnswerPair
printResults sacks = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . misplacedItemPrioritySum $ sacks
    part2 = show . groupPrioritySum $ sacks

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 3

{-# LANGUAGE OverloadedStrings #-}
module Advent2022.Day07
  ( solve,
  )
where

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair (..))
import Advent.CommonParsers (symbol, token)
import Data.Text (Text, pack)
import Text.Megaparsec (some, eof, (<|>))
import Text.Megaparsec.Char (lowerChar, newline, eol, string, letterChar, char, digitChar)

data Command = ChangeDirectory Text | DirectoryListing [File]
data File = Directory Text | RegularFile Int Text

inputParser :: Parser [Command]
inputParser = some command <* eof
  where
    command :: Parser Command
    command = symbol "$" *> (cdCommand <|> lsCommand)

    cdCommand :: Parser Command
    cdCommand = ChangeDirectory <$> (symbol "cd" *> (filename <* newline))

    lsCommand :: Parser Command
    lsCommand = DirectoryListing <$> (symbol "ls" *> listing)

    listing :: Parser [File]
    listing = some (file <* newline)

    file = directory <|> regularFile

    filename :: Parser Text
    filename = pack <$> some (char '.' <|> char '/' <|> letterChar)

    directory = Directory <$> symbol "dir" <* filename
    filesize :: Parser Int
    filesize = read <$> some digitChar
    regularFile = RegularFile <$> token filesize <*> filename

sumOfSizesOfBigDirectories :: [Command] -> Int
sumOfSizesOfBigDirectories _ = 0

printResults :: [Command] -> PuzzleAnswerPair
printResults commands = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . sumOfSizesOfBigDirectories $ commands
    part2 = ""

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 7

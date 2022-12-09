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
import qualified Data.Map as Map
import Data.Map ((!))
import Text.Megaparsec (some, eof, (<|>))
import Text.Megaparsec.Char (lowerChar, newline, eol, string, letterChar, char, digitChar)

data Command = ChangeDirectory Text | DirectoryListing [File]
data File = Directory Text | RegularFile Int Text deriving (Eq, Ord, Show)
type DirectoryTree = Map.Map [Text] [File]

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

    directory = Directory <$> (symbol "dir" *> filename)
    filesize :: Parser Int
    filesize = read <$> some digitChar
    regularFile = RegularFile <$> token filesize <*> filename

buildDirectoryTree :: [Command] -> DirectoryTree
buildDirectoryTree = f Map.empty []
  where
    f tree _ [] = tree
    f tree path (ChangeDirectory ".." : xs) = f tree (tail path) xs
    f tree path (ChangeDirectory dir : xs) = f tree (dir : path) xs
    f tree path (DirectoryListing files : xs) = f (Map.insert path files tree) path xs

allDirectorySizes :: DirectoryTree -> Map.Map [File] Int
allDirectorySizes tree = sizes [] (Directory "/")
  where
    sizes :: [File] -> File -> Map.Map [File] Int
    sizes parentPath f@(RegularFile size name) = Map.singleton (f: parentPath) size
    sizes parentPath f@(Directory name) =
      let
        children :: [File]
        children = tree ! (name : map fname parentPath)
        fname (RegularFile _ n) = n
        fname (Directory n) = n
        newKnownSizes = foldl Map.union Map.empty . map (sizes (f : parentPath)) $ children
        newKnownSizes' = Map.insert (f : parentPath) (sum . map (\x -> newKnownSizes ! (x : f : parentPath)) $ children) newKnownSizes
      in newKnownSizes'

sumOfSizesOfSmolDirectories :: [Command] -> Int
sumOfSizesOfSmolDirectories = sum . Map.elems . Map.filter (\x -> x <= 100000) . Map.filterWithKey (\k _ -> isDir k) . allDirectorySizes . buildDirectoryTree
  where
    isDir (Directory _ : _) = True
    isDir _ = False

printResults :: [Command] -> PuzzleAnswerPair
printResults commands = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . sumOfSizesOfSmolDirectories $ commands
    part2 = ""

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 7

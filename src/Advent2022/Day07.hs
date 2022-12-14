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
import Data.List (partition)
import Text.Megaparsec (some, eof, (<|>))
import Text.Megaparsec.Char (lowerChar, newline, eol, string, letterChar, char, digitChar)

data Command = ChangeDirectory Text | DirectoryListing [File]
data File = Directory Text | RegularFile Int Text deriving (Eq, Ord)
type DirectoryTree = Map.Map [File] [File] -- dir path => listing

inputParser :: Parser [Command]
inputParser = some command <* eof
  where
    command = symbol "$" *> (cdCommand <|> lsCommand)
    cdCommand = ChangeDirectory <$> (symbol "cd" *> (filename <* newline))
    lsCommand = DirectoryListing <$> (symbol "ls" *> listing)
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
    f tree path (ChangeDirectory dir : xs) = f tree (Directory dir : path) xs
    f tree path (DirectoryListing files : xs) = f (Map.insert path files tree) path xs

allDirSizes :: DirectoryTree -> Map.Map [File] Int
allDirSizes tree = sizes [Directory "/"]
  where
    sizes :: [File] -> Map.Map [File] Int
    sizes (f@(Directory _) : parentPath) =
      let
        path = f : parentPath
        children = map (\x -> x : path) . (tree !) $ path
        (childDirectories, childRegularFiles) = partition isDir children
        newKnownSizes = foldl Map.union Map.empty . map sizes $ childDirectories
        childRegularFilesSize = sum . map (\(RegularFile size _ : _) -> size) $ childRegularFiles
        childDirectorySize = sum . map (newKnownSizes !) $ childDirectories
      in Map.insert path (childRegularFilesSize + childDirectorySize) newKnownSizes


isDir :: [File] -> Bool
isDir (Directory _ : _) = True
isDir _ = False

sumOfSizesOfSmolDirectories :: [Command] -> Int
sumOfSizesOfSmolDirectories = sum . Map.elems . Map.filter (\x -> x <= 100000) . allDirSizes . buildDirectoryTree

sizeOfDirectoryToDelete :: [Command] -> Int
sizeOfDirectoryToDelete xs =
  let
    sizes = allDirSizes . buildDirectoryTree $ xs
    usedSpace = sizes ! [Directory "/"]
    freeSpace = 70000000 - usedSpace
    requiredFreeSpace = 30000000 - freeSpace
    dirs = Map.filterWithKey (\k _ -> isDir k) sizes
  in
    minimum . Map.elems . Map.filter (>= requiredFreeSpace) $ dirs

printResults :: [Command] -> PuzzleAnswerPair
printResults commands = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . sumOfSizesOfSmolDirectories $ commands
    part2 = show . sizeOfDirectoryToDelete $ commands

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 7

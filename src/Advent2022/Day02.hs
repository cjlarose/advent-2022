module Advent2022.Day02
  ( solve,
  )
where

import Advent.CommonParsers (natural, token)
import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair (..))
import Numeric.Natural (Natural)
import Data.List (sort)
import Text.Megaparsec (eof, some, sepBy1, (<|>))
import Text.Megaparsec.Char (newline, char)

data Shape = Rock | Paper | Scissors
data PlayerResponse = X | Y | Z
type GameStrat = (Shape, PlayerResponse)

inputParser :: Parser [GameStrat]
inputParser = some game <* eof

game :: Parser GameStrat
game = (,) <$> token opponentChoice <*> token playerResponse

opponentChoice :: Parser Shape
opponentChoice = Rock <$ char 'A' <|> Paper <$ char 'B' <|> Scissors <$ char 'C'

playerResponse :: Parser PlayerResponse
playerResponse = X <$ char 'X' <|> Y <$ char 'Y' <|> Z <$ char 'Z'

gameScore :: (Shape, Shape) -> Int
gameScore game = shapeScore game + outcomeScore game
  where
    shapeScore (_, Rock) = 1
    shapeScore (_, Paper) = 2
    shapeScore (_, Scissors) = 3
    outcomeScore (Rock, Rock) = 3
    outcomeScore (Rock, Paper) = 6
    outcomeScore (Rock, Scissors) = 0
    outcomeScore (Paper, Rock) = 0
    outcomeScore (Paper, Paper) = 3
    outcomeScore (Paper, Scissors) = 6
    outcomeScore (Scissors, Rock) = 6
    outcomeScore (Scissors, Paper) = 0
    outcomeScore (Scissors, Scissors) = 3

predictedScore :: [GameStrat] -> Int
predictedScore = sum . map (gameScore . (\(x, y) -> (x, makeChoice y)))
  where
    makeChoice X = Rock
    makeChoice Y = Paper
    makeChoice Z = Scissors

predictedScoreWithCorrectInterpretation :: [GameStrat] -> Int
predictedScoreWithCorrectInterpretation = sum . map (gameScore . (\(x, y) -> (x, makeChoice (x, y))))
  where
    makeChoice (Rock, X) = Scissors
    makeChoice (Rock, Y) = Rock
    makeChoice (Rock, Z) = Paper
    makeChoice (Paper, X) = Rock
    makeChoice (Paper, Y) = Paper
    makeChoice (Paper, Z) = Scissors
    makeChoice (Scissors, X) = Paper
    makeChoice (Scissors, Y) = Scissors
    makeChoice (Scissors, Z) = Rock

printResults :: [GameStrat] -> PuzzleAnswerPair
printResults games = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . predictedScore $ games
    part2 = show . predictedScoreWithCorrectInterpretation $ games

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 2

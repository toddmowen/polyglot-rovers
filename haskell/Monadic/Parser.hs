module Monadic.Parser (Scenario(..), parseScenario) where

import Text.Parsec
import Text.Parsec.Char
import Control.Monad

import Monadic.Rovers


data Scenario = Scenario { getBounds::(Int,Int), getRovers::[Rover] }
    deriving (Show, Eq)

parseScenario :: SourceName -> [Char] -> Scenario
parseScenario filePath input =
    case parse scenario filePath input of
        Left parseError -> error (show parseError)
        Right result -> result

scenario = do
    bounds <- boundsLine
    rovers <- roverLine `endBy` eof
    return (Scenario bounds rovers)

boundsLine = do
    x <- number
    space
    y <- number
    newline
    return (x,y)

roverLine = do
    x <- number
    space
    y <- number
    space
    b <- oneOf "NESW"
    newline
    return (Rover x y b)

number = read `liftM` many digit

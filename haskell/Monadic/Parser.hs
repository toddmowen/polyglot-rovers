module Monadic.Parser (Scenario(..), parseScenario) where

import Text.Parsec
import Text.Parsec.Char
import Control.Monad

import Monadic.Rovers


data Scenario = Scenario
        { getBounds::(Int,Int)
        , getRovers::[(Rover,[RoverAction])]
        }

parseScenario :: SourceName -> [Char] -> Scenario
parseScenario filePath input =
    case parse scenario filePath input of
        Left parseError -> error (show parseError)
        Right result -> result

scenario = do
    bounds <- boundsLine
    rovers <- roverSpec `endBy` eof
    return (Scenario bounds rovers)

boundsLine = do
    x <- number
    space
    y <- number
    newline
    return (x,y)

roverSpec = do
    rover <- roverLine
    commands <- commandLine
    return (rover, commands)

roverLine = do
    x <- number
    space
    y <- number
    space
    b <- bearingChar
    newline
    return (Rover x y b)

commandLine = do
    commands <- many command
    newline
    return commands

command = commandFromChar `liftM` oneOf "LRM"
number = read `liftM` many digit
bearingChar = oneOf "NESW"

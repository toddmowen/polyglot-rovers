module Monadic.Rovers where

import Control.Monad.State


data Rover = Rover { x::Int, y::Int, bearing::Char }
    deriving (Eq)

instance Show Rover where
    show (Rover x y b) = (show x) ++ " " ++ (show y) ++ " " ++ (b:"")


type RoverState = State Rover
type RoverAction = RoverState ()

runRover :: Rover -> RoverAction -> Rover
runRover rover actions = execState actions rover


-- rover commands

r,l,m :: RoverAction
r = modify $ \rover -> rover { bearing = bearing rover `successorIn` "NESWN" }
l = modify $ \rover -> rover { bearing = bearing rover `successorIn` "NWSEN" }
m = modify $ \rover ->
    case bearing rover of
        'N' -> rover { y = y rover + 1 }
        'E' -> rover { x = x rover + 1 }
        'S' -> rover { y = y rover - 1 }
        'W' -> rover { x = x rover - 1 }

x `successorIn` xs = let (_,it:successor:_) = break (==x) xs in successor 

commandsFromString :: String -> RoverAction
commandsFromString = sequence_ . map commandFromChar
commandFromChar c = case c of { 'L' -> l; 'R' -> r; 'M' -> m }


-- sample inputs from problem spec

output1 = runRover (Rover 1 2 'N') $ do l; m; l; m; l; m; l; m; m
output2 = runRover (Rover 3 3 'E') (commandsFromString "MMRMMRMRRM")

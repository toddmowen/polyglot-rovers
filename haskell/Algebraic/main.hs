#!/usr/bin/env runghc -i..

import Control.Monad
import System.IO

import Algebraic.Rovers


main = do
    getLine  -- ignore plateau bounds
    doRovers

doRovers = do
    eof <- isEOF
    unless eof $ do
        [x,y,bearing] <- liftM words getLine :: IO [String]
        program <- getLine
        let rover = Rover (read x) (read y) (read bearing)
            Rover x' y' bearing' = rover `exec` program
        putStrLn $ unwords [show x', show y', show bearing']
        doRovers

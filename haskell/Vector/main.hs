#!/usr/bin/env runghc -i..

import Vector.Rovers


main = interact (unlines . processInput . lines)
  where
    processInput (plateauBounds:roverLines) = processRovers roverLines
    processRovers [] = []
    processRovers (roverSpec:cmds:roverLines) =
        processRover roverSpec cmds : processRovers roverLines
    processRover roverSpec cmds = toSpec (fromSpec roverSpec `run` cmds)
    fromSpec roverSpec = let [xStr,yStr,[bChar]] = words roverSpec
                         in rover (read xStr) (read yStr) (charToBearing bChar)
    rover `run` cmds = foldl (flip ($)) rover $ map charToCommand cmds
    toSpec (Rover (x,y) b) = unwords [show x, show y, [bearingToUpchar b]]

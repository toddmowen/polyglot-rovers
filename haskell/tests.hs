#!/usr/bin/env runghc

import Test.QuickCheck

import Algebraic.Rovers
import Vector.Rovers
import Monadic.Rovers


prop_implementationsEquivalent x y =
    forAll bearingChar $ \bearing ->
    forAll commandString $ \cmds ->
        let algebraicResult = runAlgebraicRovers x y bearing cmds
            vectorResult = runVectorRovers x y bearing cmds
            monadicResult = runMonadicRovers x y bearing cmds
        in algebraicResult == vectorResult && algebraicResult == monadicResult
  where
    bearingChar = elements ['N', 'S', 'E', 'W']
    commandString = listOf (elements ['L', 'R', 'M'])


runAlgebraicRovers x y bearing cmds = (x', y', show bearing')
  where
    (Algebraic.Rovers.Rover x' y' bearing') =
        (Algebraic.Rovers.Rover x y (read [bearing])) `exec` cmds

runVectorRovers x y bearing cmds = (x', y', bearing')
  where
    (Vector.Rovers.Rover (x',y') b') = f (rover x y b)
    b = charToBearing bearing
    f = foldl (flip (.)) id $ map charToCommand cmds
    bearing' = bearingToUpchar b' : ""

runMonadicRovers x y bearing cmds = (x', y', b':"")
  where
    (Monadic.Rovers.Rover x' y' b') =
        runRover (Monadic.Rovers.Rover x y bearing) (commandsFromString cmds)


main = quickCheck prop_implementationsEquivalent

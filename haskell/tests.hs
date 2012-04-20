#!/usr/bin/env runghc

import Test.QuickCheck

import Algebraic.Rovers
import Vector.Rovers


prop_implementationsEquivalent x y =
    forAll bearingChar $ \bearing ->
    forAll commandString $ \cmds ->
        (algebraicResult x y bearing cmds) == (vectorResult x y bearing cmds)
  where
    bearingChar = elements ['N', 'S', 'E', 'W']
    commandString = listOf (elements ['L', 'R', 'M'])


algebraicResult x y bearing cmds = (x', y', show bearing')
  where
    (Algebraic.Rovers.Rover x' y' bearing') =
        (Algebraic.Rovers.Rover x y (read [bearing])) `exec` cmds

vectorResult x y bearing cmds = (x', y', bearing')
  where
    (Vector.Rovers.Rover (x',y') b') = f (rover x y b)
    b = charToBearing bearing
    f = foldl (flip (.)) id $ map charToCommand cmds
    bearing' = bearingToUpchar b' : ""


main = quickCheck prop_implementationsEquivalent

import Test.QuickCheck
import Data.Char (toUpper, toLower)

import AlgebraicRovers hiding (main)
import VectorRovers


prop_implementationsEquivalent x y =
    forAll bearingChar $ \bearing ->
    forAll commandString $ \cmds ->
        (algebraicResult x y bearing cmds) == (vectorResult x y bearing cmds)
  where
    bearingChar = elements ['N', 'S', 'E', 'W']
    commandString = listOf (elements ['L', 'R', 'M'])


algebraicResult x y bearing cmds = (x', y', show bearing')
  where
    (AlgebraicRovers.Rover x' y' bearing') =
        (AlgebraicRovers.Rover x y (read [bearing])) `exec` cmds

vectorResult x y upcaseBearing upcaseCmds = (x', y', upcaseBearing')
  where
    (VectorRovers.Rover (x',y') b') = f (rover x y b)
    b = charToBearing (toLower upcaseBearing)
    f = foldl (flip (.)) id $ map (charToCommand . toLower) upcaseCmds
    upcaseBearing' = toUpper (bearingToChar b') : ""


main = quickCheck prop_implementationsEquivalent

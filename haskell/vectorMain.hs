import Data.Char
import VectorRovers


main = interact (unlines . processInput . lines)
  where
    processInput (plateauBounds:roverLines) = processRovers roverLines
    processRovers [] = []
    processRovers (roverSpec:cmds:roverLines) =
        processRover roverSpec cmds : processRovers roverLines
    processRover roverSpec cmds = toSpec (fromSpec roverSpec `run` cmds)
    fromSpec roverSpec = let [xStr,yStr,[bChar]] = words roverSpec
                             x = read xStr
                             y = read yStr
                             b = charToBearing (toLower bChar)
                         in rover x y b
    rover `run` cmds = foldl (flip ($)) rover $ map (charToCommand . toLower) cmds
    toSpec (Rover (x,y) b) = unwords [show x, show y, [toUpper (bearingToChar b)]]

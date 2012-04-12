import Data.List (foldl')
import Control.Monad
import System.IO


data Bearing = N | E | S | W
    deriving (Show, Eq, Enum, Bounded, Read)

data Rover = Rover Int Int Bearing
    deriving (Show, Eq, Read)

data Command = L | R | M
    deriving (Show, Eq, Read)


left, right :: Bearing -> Bearing
left b  = if (b == minBound) then maxBound else pred b
right b = if (b == maxBound) then minBound else succ b


class Program a where
    exec :: Rover -> a -> Rover

instance Program Command where
    (Rover x y bearing) `exec` cmd =
        case cmd of
            L -> Rover x y (left bearing)
            R -> Rover x y (right bearing)
            M -> case bearing of
                E -> Rover (x+1) y bearing
                N -> Rover x (y+1) bearing
                W -> Rover (x-1) y bearing
                S -> Rover x (y-1) bearing

instance (Program a) => Program [a] where
    exec = foldl' exec

-- Allow Chars (and by extension, Strings) to be executed too.
instance Program Char where
    rover `exec` char = rover `exec` (read [char] :: Command)


-- sample inputs from problem spec
output1 = (Rover 1 2 N) `exec` [L,M,L,M,L,M,L,M,M]
output2 = (Rover 3 3 E) `exec` "MMRMMRMRRM"


main = do
    getLine  -- ignore plateau bounds
    doRovers

doRovers = do
    eof <- isEOF
    if eof then return () else do
        [x,y,bearing] <- liftM words getLine :: IO [String]
        program <- getLine
        let rover = Rover (read x) (read y) (read bearing)
            Rover x' y' bearing' = rover `exec` program
        putStrLn $ unwords [show x', show y', show bearing']
        doRovers

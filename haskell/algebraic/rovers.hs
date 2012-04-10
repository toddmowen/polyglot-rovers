data Bearing = N | E | S | W
    deriving (Show, Eq, Enum, Bounded)

data Position = Position Int Int
    deriving (Show, Eq)

data Rover = Rover Position Bearing
    deriving (Show, Eq)

data Command = L | R | M
    deriving (Show, Eq)


left, right :: Bearing -> Bearing
left b  = if (b == minBound) then maxBound else pred b
right b = if (b == maxBound) then minBound else succ b

move :: Int -> Int -> Position -> Position
move dx dy (Position x y) = Position (x+dx) (y+dy)

exec :: Rover -> Command -> Rover
exec (Rover pos bearing) cmd =
    case cmd of
        L -> Rover pos (left bearing)
        R -> Rover pos (right bearing)
        M -> case bearing of
            E -> Rover (move   1   0  pos) bearing
            N -> Rover (move   0   1  pos) bearing
            W -> Rover (move (-1)  0  pos) bearing
            S -> Rover (move   0 (-1) pos) bearing

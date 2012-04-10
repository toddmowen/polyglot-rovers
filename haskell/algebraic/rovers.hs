data Bearing = N | E | S | W
    deriving (Show, Eq, Enum, Bounded)

data Rover = Rover Int Int Bearing
    deriving (Show, Eq)

data Command = L | R | M
    deriving (Show, Eq)


left, right :: Bearing -> Bearing
left b  = if (b == minBound) then maxBound else pred b
right b = if (b == maxBound) then minBound else succ b

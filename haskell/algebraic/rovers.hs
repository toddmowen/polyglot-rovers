data Bearing = N | E | S | W
    deriving (Show, Eq, Enum, Bounded)

data Rover = Rover Int Int Bearing
    deriving (Show, Eq)

data Command = L | R | M
    deriving (Show, Eq)


left, right :: Bearing -> Bearing
left b  = if (b == minBound) then maxBound else pred b
right b = if (b == maxBound) then minBound else succ b

exec (Rover x y bearing) cmd =
    case cmd of
        L -> Rover x y (left bearing)
        R -> Rover x y (right bearing)
        M -> case bearing of
            E -> Rover (x+1) y bearing
            N -> Rover x (y+1) bearing
            W -> Rover (x-1) y bearing
            S -> Rover x (y-1) bearing

output1 = (Rover 1 2 N) `exec` L `exec` M `exec` L `exec` M `exec` L `exec` M
                        `exec` L `exec` M `exec` M

data Bearing = N | E | S | W
    deriving (Show, Eq, Enum, Bounded)

data Rover = Rover Int Int Bearing
    deriving (Show, Eq)

data Command = L | R | M
    deriving (Show, Eq)


turn :: Int -> Bearing -> Bearing
turn i b = toEnum $ wrapToBounds (fromEnum b + i)
  where
    wrapToBounds x = x `mod` (fromEnum (maxBound::Bearing) + 1)

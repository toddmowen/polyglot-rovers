module VectorRovers where

import Data.List
import Data.Maybe


-- Very simple types and operations for 2D integer vectors and matrices

type V = (Int,Int)
type M = (V,V)

plus :: V -> V -> V
(x1,x2) `plus` (y1,y2) = (x1+y1,x2+y2)

mult :: M -> V -> V
(row1,row2) `mult` v = (row1 `dot` v, row2 `dot` v)
  where
    (x1,x2) `dot` (y1,y2) = x1*y1 + x2*y2


-- rotation matrices

rotateLeft  = ( ( 0,-1)
              , ( 1, 0)
              )

rotateRight = ( ( 0, 1)
              , (-1, 0)
              )


-- velocity vectors corresponding to the compass points

bearings@[n,e,s,w] = take 4 $ iterate (mult rotateRight) (0,1)

bearingToChar b = fromJust $ lookup b (zip bearings "nesw")
charToBearing c = fromJust $ lookup c (zip "nesw" bearings)


-- rovers are represented internally by a position vector and a velocity vector,
-- but the canonical constructor syntax is: rover 1 2 n

data Rover = Rover V V

rover x y bearing = Rover (x,y) bearing

instance Show Rover where
    show (Rover (x,y) bearing) =
        "rover " ++ (show x) ++ " " ++ (show y) ++ " " ++ [bearingToChar bearing]


-- rover commands, in the form of functions

m (Rover p v) = Rover (p `plus` v) v
l (Rover p v) = Rover p (rotateLeft `mult` v)
r (Rover p v) = Rover p (rotateRight `mult` v)

charToCommand c = fromJust $ lookup c [('m',m), ('l',l), ('r',r)]


-- left-to-right function composition operator
(&) = flip (.)


-- sample inputs from problem spec
output1 = m.m.l.m.l.m.l.m.l $ rover 1 2 n    -- right-to-left syntax
output2 = m&m&r&m&m&r&m&r&r&m $ rover 3 3 e  -- left-to-right syntax

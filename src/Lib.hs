module Lib
( Point, Vector, move ) where

data Point = Point Double Double deriving (Show, Read)

data Vector = Vector Double Double deriving (Show, Read)

move :: Point -> Vector -> Point
move (Point xOffset yOffset) (Vector x y) = (Point (x + xOffset) (y + yOffset))

scale :: Vector -> Double -> Vector
scale (Vector x y) multiplier = (Vector (x * multiplier) (y * multiplier))

rotate :: Vector -> Double -> Vector
rotate vector rotation = (Vector (rotateX vector (degreeToRadian rotation)) (rotateY vector (degreeToRadian rotation)))

rotateX :: Vector -> Double -> Double
rotateX (Vector x y) rotation = x * (cos rotation) - y * (sin rotation)

rotateY :: Vector -> Double -> Double
rotateY (Vector x y) rotation = x * (sin rotation) + y * (cos rotation)

degreeToRadian :: Double -> Double
degreeToRadian degree = (degree * pi) / 180

radianToDegree :: Double -> Double
radianToDegree radian = (radian * 180) / pi

data Position = Position { loc :: Point, dir :: Vector } deriving (Show, Read)

origin = (Position (Point 0 0) (Vector 0 1))

data Command = Forward { dist :: Double } | Left { deg :: Double } | Right { deg :: Double } | Face { dir2 :: Vector } | Goto { loc2 :: Point } deriving (Show, Read)

execute :: Command -> Position -> Position
execute (Forward dist) (Position loc dir) = (Position (move loc (Vector 0 dist)) dir)
execute (Lib.Left deg) (Position loc dir) = (Position loc (rotate dir deg))
execute (Lib.Right deg) (Position loc (Vector x y)) = (Position loc (rotate (Vector (x * (-1)) (y * (-1))) deg))
execute (Face newDir) (Position loc dir) = (Position loc newDir) 
execute (Goto newLoc) (Position loc dir) = (Position newLoc dir)

executeAll :: [(Command)] -> Position -> Position
executeAll [] pos = pos
executeAll (x:xs) pos = executeAll xs (execute x pos)

knight :: Position -> Position
knight pos = executeAll [(Lib.Right 90), (Forward 10), (Lib.Left 90), (Forward 20)] pos


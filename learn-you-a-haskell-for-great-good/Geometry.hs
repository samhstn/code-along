module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidArea
, cuboidVolume
) where

import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Cube as Cube

sphereVolume :: Float -> Float
sphereVolume = Sphere.volume

sphereArea :: Float -> Float
sphereArea = Sphere.area

cubeVolume :: Float -> Float
cubeVolume = Cube.volume

cubeArea :: Float -> Float
cubeArea = Cube.area

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume = Cuboid.volume

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea = Cuboid.area

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b

data Shape = Circle Float Float Float | Rectangle Float Float Float Float

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectanble x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

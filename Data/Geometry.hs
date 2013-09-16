module Data.Geometry (
    LinearRing
  , Geometry (..)
  , pointInside
  , Vector2 (..)
  , Vector3 (..)
) where

import Data.List (foldl')
import Data.Vector.V2 (Vector2(..))
import Data.Vector.V3 (Vector3(..))
import Graphics.Triangulation.Delaunay (triangulate)

epsilon = 1e-6

pointInside :: [Vector2] -> Vector2 -> Bool
pointInside poly v
    | v `touches` poly = False
    | otherwise = foldl' crosses False $ segments poly
  where
    crosses r (j,i) = let cond = ((v2y i > v2y v) /= (v2y j > v2y v)) &&
                                 (v2x v < (v2x j - v2x i) *
                                          (v2y v - v2y i) /
                                          (v2y j - v2y i) + v2x i)
                      in if cond then not r else r
    touches v = any (pointInSegment v) . segments
    pointInSegment v (a, b)
        | isHorizontal      =  v2x v == v2x a
        | isVertical        =  v2y v == v2y a
        | v `between` (a,b) =  abs diff < epsilon
        | otherwise         =  False
      where
        isHorizontal = v2x (b-a) == 0 
        isVertical   = v2y (b-a) == 0 
        between (Vector2 x y) (a',b') = x0 <= x && x < x1 && y0 <= y && y < y1
           where v0 = Vector2 (min (v2x a') (v2x b')) (min (v2y a') (v2y b')) 
                 v1 = Vector2 (max (v2x a') (v2x b')) (max (v2y a') (v2y b')) 
                 Vector2 x0 y0 = v0
                 Vector2 x1 y1 = v1
        diff = v2y v - v2y a - slope * (v2x v - v2y a)
        slope = v2y (b-a) / v2x (b-a)

segments :: Eq a => [a] -> [(a, a)]
segments [] = []
segments poly
    | isClosed poly = zip poly (tail poly)
    | otherwise     = zip poly (tail poly ++ [head poly])
  where
    isClosed poly = head poly == head (reverse poly)

type LinearRing a = [a]

data Geometry a =
    Polygon {
        outerBoundary :: LinearRing a
      , innerBoundary :: [LinearRing a]
      }
   | MultiGeometry [Geometry a]
 deriving (Eq, Show)
               
vector2D :: Vector3 -> Vector2
vector2D (Vector3 x y _) = Vector2 x y

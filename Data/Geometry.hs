module Data.Geometry (
    LinearRing
  , Face
  , Surface
  , Geometry (..)
  , pointInside
  , extrude
  , Vector2 (..)
  , Vector3 (..)
) where

import Data.List (foldl', nub, sort)
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

class Eq a => Vector2D a where
    to2D :: a -> Vector2
               
instance Vector2D Vector3 where
    to2D (Vector3 x y _) = Vector2 x y

instance Vector2D Vector2 where
    to2D = id

to3D :: Double -> Vector2 -> Vector3
to3D z (Vector2 x y) = Vector3 x y z

newtype Face a = Face (a, a, a) deriving (Show)

type Surface = [Face Vector3]

extrude :: Vector2D a => Double -> Geometry a -> Surface
extrude h (MultiGeometry gs) = concat $ map (extrude h) gs
extrude h (Polygon ob []) = floor ++ ceiling ++ walls
  where
    floor = map (fmap sink) $ nub $ map Face $ triangulate ob2d
    ceiling = map (fmap rise) $ nub $ map Face $ triangulate ob2d
    walls = concat $ map wall $ segments ob2d
    wall (a,b) = [ Face (rise a, rise b, sink a)
                 , Face (sink a, sink b, rise b)]
    rise = to3D h
    sink = to3D 0
    ob2d = map to2D ob
extrude h (Polygon ob ib) = undefined


instance Functor Face where
    fmap f (Face (a,b,c)) = Face (f a, f b, f c)

instance Eq a => Eq (Face a) where
    (Face (a1, a2, a3)) == (Face (b1, b2, b3)) =
         (a1==b1 || a1==b2 || a1==b3)
      && (a2==b1 || a2==b2 || a2==b3)
      && (a3==b1 || a3==b2 || a3==b3) 

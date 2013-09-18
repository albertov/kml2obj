{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}
module Data.Geometry (
    LinearRing
  , Face (..)
  , Surface (..)
  , Geometry (..)
  , pointInside
  , extrude
  , surfaceVertices
  , isMulti
  , Vector2 (..)
  , Vector3 (..)
) where

import Data.List (foldl', nub, sort)
import Data.Monoid
import Data.Vector.V2 (Vector2(..))
import Data.Vector.V3 (Vector3(..))
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import qualified Data.Vector.Generic.Base as V
import qualified Data.Vector.Unboxed as U
import qualified Data.HashSet as S
import Data.Hashable (Hashable)

epsilon = 1e-6

pointInside :: LinearRing Vector2 -> Vector2 -> Bool
pointInside poly v
    | v `touches` poly = False
    | otherwise = U.foldl' crosses False $ segments poly
  where
    crosses r (j,i) = let cond = ((v2y i > v2y v) /= (v2y j > v2y v)) &&
                                 (v2x v < (v2x j - v2x i) *
                                          (v2y v - v2y i) /
                                          (v2y j - v2y i) + v2x i)
                      in if cond then not r else r
    touches v = U.any (pointInSegment v) . segments
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

segments :: (Eq a, U.Unbox a) => LinearRing a -> U.Vector (a, a)
segments poly
    | U.null poly   = U.empty
    | isClosed poly = U.zip poly (U.tail poly)
    | otherwise     = U.zip poly (U.tail poly `U.snoc` U.unsafeHead poly)
  where
    isClosed poly = U.unsafeHead poly == U.unsafeLast poly

type LinearRing = U.Vector

data Geometry a =
    Polygon {
        outerBoundary :: LinearRing a
      , innerBoundary :: [LinearRing a]
      }
   | MultiGeometry [Geometry a]
 deriving (Eq, Show)

isMulti (MultiGeometry _) = True
isMulti _ = False

class (Ord a, U.Unbox a) => Coord a where
    to2D :: a -> Vector2
    to3D :: Double -> a -> Vector3
               
newtype Face a = Face {faceVertices :: LinearRing a} deriving Show

newtype Surface a = Surface {faces :: [Face a]} deriving Show

surfaceVertices :: (U.Unbox a, Hashable a, Ord a) => Surface a -> [a]
surfaceVertices =
    S.toList . S.fromList . concat . map (U.toList . faceVertices) . faces

extrude :: Coord a => Double -> Geometry a -> [Surface Vector3]
extrude h (MultiGeometry gs) = concat $ map (extrude h) gs
extrude h (Polygon ob _)
    | U.null ob = []
    | otherwise = [floor <> ceiling <> walls]
  where
    floor, ceiling, walls :: Surface Vector3
    floor = Surface $ [Face $ U.map sink ob2d]
    ceiling = Surface $ [Face $ U.map rise ob2d]
    walls = mconcat $ map wall $ U.toList $ segments ob2d
    wall (a, b) = Surface [face [rise a, rise b, sink b, sink a, rise a]]
    rise = to3D h
    sink = to3D 0
    ob2d = U.map to2D ob
    face = Face . U.fromList
-- extrude h (Polygon ob ib) = undefined


--
-- Instances
-- 
--
instance Monoid (Surface a) where
  mempty = Surface []
  (Surface a) `mappend` (Surface b) = Surface (a `mappend` b)


instance Coord Vector3 where
    to2D (Vector3 x y _) = Vector2 x y
    to3D _ = id

instance Coord Vector2 where
    to2D = id
    to3D z (Vector2 x y) = Vector3 x y z


instance Ord Vector3 where
  (Vector3 a b c) `compare` (Vector3 a' b' c') = (a,b,c) `compare` (a',b',c')

instance Ord Vector2 where
  (Vector2 a b) `compare` (Vector2 a' b') = (a,b) `compare` (a',b')

derivingUnbox "Vector2"
    [t| Vector2 -> (Double, Double) |]
    [| \ (Vector2 x y) -> (x, y) |]
    [| \ (x, y) -> Vector2 x y |]

derivingUnbox "Vector3"
    [t| Vector3 -> (Double, Double, Double) |]
    [| \ (Vector3 x y z) -> (x, y, z) |]
    [| \ (x, y, z) -> Vector3 x y z |]

{-# LANGUAGE BangPatterns #-}
module Data.GeoReference (
    GeoReference (..)
  , Pixel (..)
  , Point (..)
  , SubPixel (..)
  , Box (..)
  , Extent
  , Shape

  , isEmpty

  , mkBox
  , mkShape
  , mkExtent

  , width
  , height

  , forward
  , backward
  , forwardS
  , backwardS
  , intersection
  , intersects
  , union
  , scale
) where

import           Control.Applicative ((<$>), (<*>))
import           Data.Monoid (Monoid(..))


data GeoReference = GeoReference {    
    extent :: !Extent
  , shape  :: !Shape
  } deriving (Eq, Show)



data Box a = Box {
    minx :: !a
  , miny :: !a
  , maxx :: !a
  , maxy :: !a
} deriving (Eq, Show)

instance (Ord a, Num a) => Monoid (Box a) where
    mempty = emptyBox
    mappend = union

emptyBox :: (Num a, Ord a) => Box a
emptyBox = Box 0 0 0 0

isEmpty :: (Num a, Ord a) => Box a -> Bool
isEmpty = (==) emptyBox

mkBox :: (Ord a, Num a)
  => a -> a -> a -> a ->  Box a
mkBox x0 y0 x1 y1
    | x1>x0 && y1>y0 = Box x0 y0 x1 y1
    | otherwise      = emptyBox

intersection :: (Ord a, Num a) => Box a -> Box a -> Box a
intersection a b
    | intersects a b = Box (max (minx a) (minx b))
                           (max (miny a) (miny b))
                           (min (maxx a) (maxx b))
                           (min (maxy a) (maxy b))
    | otherwise      = emptyBox

union :: (Ord a, Num a) => Box a -> Box a -> Box a
union a b
    | isEmpty a = b
    | isEmpty b = a
    | otherwise
        = Box (min (minx a) (minx b))
              (min (miny a) (miny b))
              (max (maxx a) (maxx b))
              (max (maxy a) (maxy b))

intersects :: (Ord a, Num a) => Box a -> Box a -> Bool
intersects a b = not ( (minx a > maxx b) ||
                       (maxx a < minx b) ||
                       (miny a > maxy b) ||
                       (maxy a < miny b) )
{-# INLINE intersects #-}


data Pixel = Pixel !Int !Int deriving (Eq, Show)
data SubPixel = SubPixel !Double !Double  deriving (Eq, Show)
data Point = Point !Double !Double  deriving (Eq, Show)

toSPixel :: Pixel -> SubPixel
toSPixel (Pixel x y) = SubPixel (fromIntegral x) (fromIntegral y)
{-# INLINE toSPixel #-}

fromSPixel :: SubPixel -> Pixel
fromSPixel (SubPixel x y) = Pixel (round x) (round y)
{-# INLINE fromSPixel #-}

width, height :: Num a => Box a -> a
width  b = maxx b - minx b
{-# SPECIALIZE INLINE width :: Extent -> Double #-}
{-# SPECIALIZE INLINE width :: Shape -> Int #-}

height b = maxy b - miny b
{-# SPECIALIZE INLINE height :: Extent -> Double #-}
{-# SPECIALIZE INLINE height :: Shape -> Int #-}

type Shape = Box Int

mkShape :: Int -> Int -> Shape
mkShape = mkBox 0 0

type Extent = Box Double

mkExtent :: Double -> Double -> Double -> Double ->  Extent
mkExtent = mkBox


forwardS :: GeoReference -> Point -> SubPixel
forwardS ctx (Point x y) = SubPixel ln col
    where col     = (x - minx e) * sx
          ln      = (maxy e - y) * sy
          (sx,sy) = scale ctx
          e       = extent ctx

scale :: GeoReference -> (Double, Double)
scale ctx = (sx,sy)
  where sx = fromIntegral (width s) / width e
        sy = fromIntegral (height s) / height e
        s  = shape ctx
        e  = extent ctx
{-# INLINE scale #-}

backwardS :: GeoReference -> SubPixel -> Point
backwardS ctx (SubPixel ln col) = Point x y
    where x       = minx e + col / sx
          y       = maxy e - ln / sy
          (sx,sy) = scale ctx
          e       = extent ctx

forward :: GeoReference -> Point -> Pixel
forward ctx = fromSPixel . forwardS ctx
{-# INLINE forward #-}

backward :: GeoReference -> Pixel -> Point
backward ctx = backwardS ctx . toSPixel
{-# INLINE backward #-}

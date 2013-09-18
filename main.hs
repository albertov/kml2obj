{-# LANGUAGE StandaloneDeriving, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Main (main) where
import System.IO
import System.Environment
import Control.Monad
import Control.Exception
import Text.XML.Kml
import Text.Wavefront
import Data.Geometry
import Data.GeoReference
import Control.Monad
import Data.Monoid
import Data.List (foldl')
import Data.String (fromString)
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Lazy as BS
import Data.Vector.V2 (Vector2(..))
import Geo.Proj4
import Data.List.Split (chunksOf)


main = do
    [input, output] <- getArgs
    KmlDocument placemarks <- parseKmlFile input
    let geoms = map pGeometry placemarks
        ext = foldl' (<>) mempty $ map geometryExtent geoms'
        georef = GeoReference ext (Box (-5) (-5) 5 (round (10*ratio) - 5))
        ratio = height ext / width ext
        geoms' = map (reprojectGeom f) geoms
        geoms'' = map (reprojectGeom f2) geoms'
        pFaces = map (\g -> extrude (getHeight g) g) geoms'
        objs = map (\(p,fs) -> Object fs (pId p)) $ zip placemarks pFaces
        pj = newProjection "+init=epsg:25831"
        f (Vector3 x y z) = Vector3 x' y' z
          where (x', y') = pjFwd pj (x*pi/180, y*pi/180)
        f2 (Vector3 x y z) = Vector3 x' y' (z * (fst $ scale georef))
          where SubPixel x' y' = forwardS georef $ Point x y
    forM_ (zip [1..] $ chunksOf 100 objs) $ \(ix, chunk) -> do
        withFile (show ix ++ "." ++ output) WriteMode $ \h -> do
          BS.hPut h $ writeObjects chunk

getHeight (MultiGeometry []) = 0
getHeight (MultiGeometry (x:_)) = getHeight x
getHeight (Polygon ob _)
    | U.null ob = 0
    | otherwise = let Vector3 _ _ z = U.unsafeHead ob in z

reprojectGeom f (MultiGeometry g) = MultiGeometry (map (reprojectGeom f) g)
reprojectGeom f (Polygon ob ibs) = Polygon (U.map f ob) (map (U.map f) ibs)

geometryExtent :: Coord a => Geometry a -> Box Double
geometryExtent (MultiGeometry gs) = foldl' (<>) mempty $ map geometryExtent gs
geometryExtent (Polygon ob _) = Box {
    minx = U.minimum . U.map v2x . U.map to2D $ ob
  , miny = U.minimum . U.map v2y . U.map to2D $ ob
  , maxx = U.maximum . U.map v2x . U.map to2D $ ob
  , maxy = U.maximum . U.map v2y . U.map to2D $ ob
}

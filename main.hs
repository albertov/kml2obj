{-# LANGUAGE StandaloneDeriving, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Main (main) where
import System.IO
import System.Environment
import Control.Monad
import Control.Exception
import Text.XML.Kml
import Text.Wavefront
import Data.Geometry
import Control.Monad
import qualified Data.ByteString.Lazy as BS


main = do
    [input, output] <- getArgs
    h <- openFile input ReadMode
    kml <- liftM parseKmlBS $ BS.hGetContents h
    case kml of
      Left e -> error e
      Right (KmlDocument ps) -> do
        let geoms    = map pGeometry ps
            surfaces = map (\g -> extrude (getHeight g) g) geoms
        withFile output WriteMode $ \h -> do
            BS.hPut h (writeSurfaces $ concat surfaces)

getHeight (MultiGeometry []) = 0
getHeight (MultiGeometry (x:_)) = getHeight x
getHeight (Polygon [] _) = 0
getHeight (Polygon (Vector3 _ _ z:_) _) =  z

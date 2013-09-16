{-# LANGUAGE OverloadedStrings #-}

module Text.XML.Kml (
    KmlDocument (..)
  , Placemark (..)
  , parseKml
  , parseKmlFile
  , parseCoordinates
) where

import Prelude hiding (readFile, writeFile)
import Data.Geometry
import Text.XML
import Text.XML.Cursor
import Data.Text (Text)
import Control.Applicative (Applicative,
                            (<*>),
                            (*>),
                            (<*),
                            (<$>),
                            (<|>),
                            pure)
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Data.Text (Text)
import Data.Char (isSpace)
import Data.Default (def)
import qualified Data.Text as T
import Data.Text.Lazy (fromStrict)

data KmlDocument = KmlDocument {
    kmlPlacemarks :: [Placemark]
} deriving (Eq, Show)

data Placemark = Placemark {
    pGeometry :: Geometry Vector3
} deriving (Eq, Show)

parseKml :: (Monad m, Applicative m) => Text -> m KmlDocument
parseKml t = do
    doc <- liftEither $ parseText def (fromStrict t)
    parseDoc doc

parseKmlFile path = do
    doc <- readFile def path
    case parseDoc doc of
      Left e -> fail e
      Right a -> return a

parseDoc doc = do
    let pNodes  = fromDocument doc $// laxElement "Polygon"
    polygons <- mapM (parsePolygon . node) pNodes
    return $ KmlDocument $ map Placemark polygons
  where
    parsePolygon p = Polygon <$> oBoundary <*> lRings
      where
        oBoundary = do
          coords <- parseCoords obCoords
          case coords of
            [cs] -> return cs
            []   -> fail "No outer boundary"
            _    -> fail "Multiple outer boundaries"
        lRings = parseCoords ibCoords
        c = fromNode p
        parseCoords = mapM (liftEither . parseCoordinates . T.strip)
        obCoords = fromNode p $// laxElement "outerBoundaryIs"
                              &// laxElement "coordinates"
                              &// content
        ibCoords = fromNode p $// laxElement "innerBoundaryIs"
                              &// laxElement "coordinates"
                              &// content

parseCoordinates :: Text -> Either String [Vector3]
parseCoordinates = parseOnly (coordinates <* endOfInput)

coordinates :: Parser [Vector3]
coordinates = coordinate `sepBy1` (char ' ') 
  where
    coordinate = coord3d <|> coord2d
    coord3d = Vector3 <$> double <*> "," .*> double <*> "," .*> double
    coord2d = Vector3 <$> double <*> "," .*> double <*> pure 0

liftEither (Right a) = return a
liftEither (Left a) = fail $ show a

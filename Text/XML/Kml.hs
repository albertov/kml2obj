{-# LANGUAGE OverloadedStrings #-}

module Text.XML.Kml (
    KmlDocument (..)
  , Placemark (..)
  , parseKml
  , parseKmlBS
  , parseKmlFile
  , parseCoordinates
  , placemarkConduit
) where

import Prelude hiding (readFile, writeFile)
import Data.Geometry
import Text.XML
import Text.XML.Cursor
import Data.XML.Types (Event)
import qualified Text.XML.Stream.Parse as X
import Data.Text (Text)
import Control.Applicative (Applicative,
                            (<*>),
                            (*>),
                            (<*),
                            (<$>),
                            (<|>),
                            pure)
import Control.Monad (liftM)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Attoparsec.Text
import Data.ByteString.Lazy (ByteString)
import Data.Attoparsec.Combinator
import Data.Text (Text)
import Data.Char (isSpace)
import Data.Default (def)
import qualified Data.Text as T
import Data.Text.Lazy (fromStrict)
import Data.Conduit

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

parseKmlBS :: (Monad m, Applicative m) => ByteString -> m KmlDocument
parseKmlBS t = do
    doc <- liftEither $ parseLBS def t
    parseDoc doc

parseKmlFile path = do
    doc <- readFile def path
    case parseDoc doc of
      Left e -> fail e
      Right a -> return a

kName :: Text -> Name
kName n = Name n (Just "http://www.opengis.net/kml/2.2") (Just "kml")

kElement = element . kName

placemarkConduit :: MonadThrow m => Consumer Event m [Placemark]
placemarkConduit = X.force "kml" $ X.tagName (kName "kml") X.ignoreAttrs $ \_ ->
    X.force "Document" $ X.tagName (kName "Document") X.ignoreAttrs $ \_ ->
      concat <$> (X.many $ X.tagName (kName "Folder") X.ignoreAttrs $ \_ ->
        X.many parsePlacemark)
  where
    parsePlacemark :: MonadThrow m => Consumer Event m (Maybe Placemark)
    parsePlacemark = X.tagName (kName "Placemark") X.ignoreAttrs $ \_ ->
        Placemark <$> X.force "Missing geometry" parseGeometry

    parseGeometry = X.force "Placemark must have one geometry" $ X.choose [
        X.tagNoAttr (kName "MultiGeometry") $ parseMuligeometry
      , X.tagNoAttr (kName "Polygon") $ parsePolygon
      ]

    parseGeometry, parsePolygon, parseMuligeometry ::
        MonadThrow m => Consumer Event m (Maybe (Geometry Vector3))
    parsePolygon = undefined
    parseMuligeometry = undefined
          

parseDoc :: (Monad m, Applicative m) => Document -> m KmlDocument
parseDoc doc = do
    let pNodes     = fromDocument doc $// kElement "Placemark"
        placemarks = catMaybes $ map (parsePlacemark . node) pNodes
    return $ KmlDocument placemarks
  where
    parsePlacemark :: (Monad m, Applicative m) => Node -> m Placemark
    parsePlacemark p = do
        geoms <- parseGeoms p
        case geoms of
          [g] -> return $ Placemark g
          _   -> fail "Expected only one geometry per placemark"

    parseGeoms :: (Monad m, Applicative m) => Node -> m [Geometry Vector3]
    parseGeoms p = (<>) <$> mapM (parseMulti . node) multis
                        <*> mapM (parsePolygon . node) polys
      where
        multis = fromNode p $/ kElement "MultiGeometry"
        polys = fromNode p $/ kElement "Polygon"

    parseMulti p = MultiGeometry <$> parseGeoms p

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
        obCoords = fromNode p $/ kElement "outerBoundaryIs"
                              &// kElement "coordinates"
                              &/ content
        ibCoords = fromNode p $/ kElement "innerBoundaryIs"
                              &// kElement "coordinates"
                              &/ content

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

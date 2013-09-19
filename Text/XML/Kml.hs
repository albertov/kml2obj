{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Text.XML.Kml (
    KmlDocument (..)
  , Placemark (..)
  , parseKml
  , parseKmlFile
  , parseCoordinates
) where

import Prelude hiding (readFile, writeFile)
import System.IO (openFile, IOMode(..))
import Data.Geometry
import Data.ByteString.Lazy (ByteString, hGetContents)
import Data.Text (Text)
import Control.Applicative (Applicative,
                            (<*>),
                            (*>),
                            (<*),
                            (<$>),
                            (<|>),
                            pure)
import Control.Monad (liftM, when)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Data.Text (Text)
import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import Text.XML.Expat.Tree (UNode, NodeG(..), parseThrowing, defaultParseOptions)


data KmlDocument = KmlDocument {
    kmlPlacemarks :: [Placemark]
} deriving (Eq, Show)

data Placemark = Placemark {
    pGeometry :: !(Geometry Vector3)
  , pId :: Maybe Text
} deriving (Eq, Show)


parseKmlFile :: FilePath -> IO KmlDocument
parseKmlFile name = openFile name ReadMode >>= hGetContents  >>= parseKml

parseKml :: (Monad m, Applicative m) => ByteString -> m KmlDocument
parseKml = parseTree . parseThrowing defaultParseOptions
  where
    parseTree :: (Monad m, Applicative m) => UNode Text -> m KmlDocument
    parseTree = return . KmlDocument . catMaybes . extractPlacemarks

    extractPlacemarks e@(Element tag _ children)
        | tag `elem` ["kml", "Document", "Folder"]
            = concat $ map extractPlacemarks children
        | tag == "Placemark" = [parsePlacemark e]
        | otherwise          = []
    extractPlacemarks _ = []

    parsePlacemark e@(Element _ attrs cs) = do
        let id' = lookupAttr attrs "id"
        geometries <- mapM parseGeometry $ filter isGeomNode cs
        case geometries of
          [g] -> return $ Placemark {pGeometry=g, pId=id'}
          _   -> fail "Expected only one geometry per placemark"
    parsePlacemark _ = Nothing

    parseGeometry (Element "MultiGeometry" _ cs) = do
        subgeoms <- mapM parseGeometry $ filter isGeomNode cs
        return $ MultiGeometry subgeoms

    parseGeometry (Element "Polygon" _ cs) = do
        e <- lookupChild cs "outerBoundaryIs"
        obCoords <- mapM parseLinearRing $ childElements e
        when (length obCoords < 1) $
          fail "outer boundary should have one ring"
        when (length obCoords > 1) $
          fail "outer boundary should have only one ring"
        let ibE = lookupChild cs "innerBoundaryIs"
        ibCoords <- case ibE of
                      Just e' -> mapM parseLinearRing (childElements e')
                      Nothing -> return []
        return $ Polygon (head obCoords) ibCoords

    parseGeometry (Element name _ _) =
        fail $ "Geometry '" ++ (T.unpack name) ++ "' not supported"

    isGeomNode (Element tag _ _)
        | tag `elem` ["MultiGeometry", "Polygon"] = True
        | otherwise                               = False
    isGeomNode _ = False

    childElements (Element _ _ cs) = filter isElement cs
    childElements _ = []

    isElement (Element _ _ _) = True
    isElement _               = False

    parseLinearRing (Element "LinearRing" _ cs) = do
      Element _ _ cs <- lookupChild cs "coordinates"
      let t = T.concat $ map (\(Text t) -> t) cs
      liftEither $ parseCoordinates $ T.strip t
    parseLinearRing _ = fail "Not a LinearRing node"

    lookupAttr attrs name =
      case filter ((==) name . fst) attrs of
        (_,v):_ -> return v
        _       -> fail $ "Attribute not found: " ++ (T.unpack name)

    lookupChild children name =
      case filter (matchTag name) children of
        x:_ -> return x
        _   -> fail $ "Child not found: " ++ (T.unpack name)

    matchTag name (Element tag _ _) = name==tag
    matchTag _ _ = False


parseCoordinates :: Text -> Either String (LinearRing Vector3)
parseCoordinates = parseOnly (coordinates <* endOfInput)

coordinates :: Parser (LinearRing Vector3)
coordinates = U.fromList <$> coordinate `sepBy1` (char ' ') 
  where
    coordinate = coord3d <|> coord2d
    coord3d = Vector3 <$> double <*> "," .*> double <*> "," .*> double
    coord2d = Vector3 <$> double <*> "," .*> double <*> pure 0

liftEither (Right a) = return a
liftEither (Left a) = fail $ show a

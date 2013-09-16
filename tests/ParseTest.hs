{-# LANGUAGE OverloadedStrings #-}
module ParseTest where

import Test.Hspec
import Data.Geometry
import Text.XML.Kml

parseSpecs :: Spec
parseSpecs =
  describe "KML parsing tests" $ do
    it "parses a simple document" $ do
      doc <- parseKmlFile "tests/data/placemark.kml" :: IO KmlDocument
      length (kmlPlacemarks doc) `shouldBe` 1
      let geom = pGeometry $ head $ kmlPlacemarks doc
      length (innerBoundary geom) `shouldBe` 2

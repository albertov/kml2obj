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
      length (kmlPlacemarks doc) `shouldBe` 2
      let [g1, g2] = map pGeometry $ kmlPlacemarks doc
      isMulti g1 `shouldBe` True
      isMulti g2 `shouldBe` False
      length (innerBoundary g2) `shouldBe` 2

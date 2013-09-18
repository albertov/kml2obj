module GeometryTest where

import Test.Hspec
import Data.Geometry

geometrySpecs :: Spec
geometrySpecs =
  describe "geometry utilities tests" $ do
    describe "pointInside" $ do

        let poly = fromList [Vector2 0 0, Vector2 0 1, Vector2 1 1, Vector2 1 0]

        it "returns False for points outside" $ do
            let results = map (pointInside poly) points
                points = [Vector2 2 2, Vector2 (-2) (-1)]
            all ((==) False) results `shouldBe` True

        it "returns False for touching points" $ do
            let results = map (pointInside poly) points
                points  =  toList poly
                        ++ [Vector2 0.5 y | Vector2 _ y <- toList poly]
                        ++ [Vector2 x 0.5 | Vector2 x _ <- toList poly]
            all ((==) False) results `shouldBe` True

        it "returns True for points strictly inside" $ do
            let results = map (pointInside poly) points
                points = [Vector2 0.1 0.1, Vector2 0.9 0.9]
            all ((==) True) results `shouldBe` True

    describe "extrude" $ do
        let ob = fromList [Vector2 0 0, Vector2 0 1, Vector2 1 1, Vector2 1 0]
            ib = fromList [Vector2 0.25 0.25, Vector2 0.75 0.75, Vector2 0.75 0.25]
            poly = Polygon ob []
            poly2 = Polygon ob [ib]
        it "extrudes polygon without inner boundary" $ do
            let faces = extrude 10 poly
            length faces `shouldBe` ( 1  -- ceiling
                                    + 1  -- floor
                                    + 4) -- each wall

        it "extrudes polygon with inner boundary (TODO)" $ do
            let faces = extrude 10 poly2
            length faces `shouldBe` ( 1  -- ceiling
                                    + 1  -- floor
                                    + 4) -- each wall


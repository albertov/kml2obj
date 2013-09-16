import Test.Hspec (hspec)

import ParseTest (parseSpecs)
import GeometryTest (geometrySpecs)

main :: IO ()
main = hspec $ do
  parseSpecs
  geometrySpecs

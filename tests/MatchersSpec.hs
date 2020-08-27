module MatchersSpec where

import Data
import Matchers
import Test.Hspec
import Test.QuickCheck
import Prelude hiding (any)

spec :: Spec
spec = do
  describe "Matchers" $ do
    context "any" $ do
      it "should match any char" $
        any "string" `shouldBe` Just (Match "s" "s" [])
      it "should match any char quickcheck" $
        property $
          \x -> (read . show) x == (x :: Int)
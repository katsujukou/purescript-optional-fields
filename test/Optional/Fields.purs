module Test.Optional.Fields where

import Prelude

import Data.Maybe (Maybe(..))
import Optional.Fields (optional)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type T =
  ( foo :: Int
  , bar :: String
  , baz ::
      { quz :: Maybe Number
      , qux :: Array Boolean
      }
  )

spec :: Spec Unit
spec = describe "Optional.Fields" do
  describe "optional" do
    it "should map `a` to `Maybe a`" do
      optional 42 `shouldEqual` (Just 42)

    it "should map `Maybe a` to `Maybe a`." do
      optional (Just true) `shouldEqual` Just true
      optional (Nothing :: _ String) `shouldEqual` Nothing
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
    it "should convert `a` to `Maybe a`" do
      optional @Int 42 `shouldEqual` (Just 42)

    it "should leave `Maybe a` as-is." do
      optional @Boolean (Just true) `shouldEqual` (Just true)
      optional @String (Nothing :: Maybe String) `shouldEqual` (Nothing :: Maybe String)

    it "should map every field to `Maybe`ed type" do
      optional @{ | T } {} `shouldEqual`
        ( Just
            { foo: Nothing
            , bar: Nothing
            , baz: Nothing
            }
        )

      optional @{ | T } { foo: 42 } `shouldEqual`
        ( Just
            { foo: Just 42
            , bar: Nothing
            , baz: Nothing
            }
        )

      optional @{ | T } { foo: 42, baz: {} } `shouldEqual`
        ( Just
            { foo: Just 42
            , bar: Nothing
            , baz: Just { quz: Nothing, qux: Nothing }
            }
        )

      optional @{ | T } { foo: 42, baz: { quz: Just 2.718 } } `shouldEqual`
        ( Just
            { foo: Just 42
            , bar: Nothing
            , baz: Just { quz: Just (Just 2.718), qux: Nothing }
            }
        )

      optional @{ | T } { foo: 42, bar: "x", baz: { quz: Nothing :: _ Number, qux: [ true, false, false ] } } `shouldEqual`
        ( Just
            { foo: Just 42
            , bar: Just "x"
            , baz: Just { quz: Nothing :: _ (_ Number), qux: Just [ true, false, false ] }
            }
        )
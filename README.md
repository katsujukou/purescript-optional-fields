# purescript-record-optional-fields

Type-safe manipulation of optional fields for PureScript record types.

[![purs - v0.15.13](https://img.shields.io/badge/purs-v0.15.13-blue?logo=purescript)](https://github.com/purescript/purescript/releases/tag/v0.15.13) [![CI](https://github.com/katsujukou/purescript-optional-fields/actions/workflows/ci.yml/badge.svg)](https://github.com/katsujukou/purescript-optional-fields/actions/workflows/ci.yml)

## What's this?

`purescript-record-optional-fields` is a library that offers a flexible, yet type-safe way of handling records with optional fields.

## How to use?

Suppose you have following type:

```purescript
type T =
  { foo :: Int
  , bar :: String
  , baz ::
    { quz :: Number
    , qux :: Array Boolean
    }
  }
```

Using the `optional` function provided by this library, you can convert a record with some missing fields for the target type `T` into a record that includes all the fields from `T`, but the types of the fields are modified with the `Maybe` type constructor.

```purescript
-- Example 1
optional @T {}
  == Just { foo: Nothing, bar: Nothing, baz: Nothing }

-- Example 2
optional @T { foo: 42 }
  == Just
    { foo: Just 42
    , bar: Nothing
    , baz: Nothing
    }
```

If the target type T has nested record fields, all of its nested fields will be recursively transformed into their Maybe counterparts, as illustrated in the following example.

```purescript
-- Example 3
optional @T { foo: 42, baz: { quz: 3.14 } }
  == Just
    { foo: Just 42
    , bar: Nothing
    , baz: Just
      { quz: Just 3.14
      , qux: Nothing
      }
    }
```

Sometimes, you may want to include a certain field only if a specific conditional check is met. In this case, you can provide a value with an explicit Maybe modification.

```purescript
-- Example 4
optional @T
  { foo: if (some conditional check)
         then Just 42 else Nothing
  }
  == Just
    { foo: Just 42  -- or Nothing, if condition does not hold
    , bar: Nothing
    , baz: Nothing
    }
```

## Custom compiler error

When you attempt to convert a record with fields not belonging to the target type, the compiler will indicate the issue with a user-friendly custom error.

```purescript
optional @T { foo: "abc" }
```

will result in following error:

```
Error found:

Custom error:

  An error occurred while checking input option type:

  - At field `foo`:
    Unexpected value type: `String`

  💡 You must specify a value of type `Int`.
```

And this case

```purs
optional @T { baz: { qux: [true] , quux: [3.14, 2.718] } }
```

will result in:

```
Custom error:

  An error occurred while checking input option type:

  - At field `baz`:
    Unsupported field name: `quux`.

  💡 Supported fields include: `qux` `quz`
```

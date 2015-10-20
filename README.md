# n-vector

Fixed size vectors using type-level naturals

This package aims to replicate most of the api exposed by the `Data.Vector.Generic` module of the `vector` package in a typesafe way.
Some functions, such as `slice`, have different types:

```haskell
-- Data.Vector.Generic
slice :: Vector v a => Int -> Int -> v a -> v a

-- Data.Vector.Generic.Fixed
slice :: ( KnownNat start
         , KnownNat length
         , KnownNat total
         , GT ~ CmpNat total (start + length)
         , G.Vector v a
         ) => Proxy# start -> Proxy# length -> V total v a -> V length v a
```

The latter's type signature looks a bit scary, but the type safety is sometimes worth it.
What's more, not only is the `newtype` wrapper `V` free, but `Proxy#`'s are as well (unlike `Int`'s...).

It is worth noting that some functions from `vector`, such as `filter`, have no direct analog in the fixed-length world.


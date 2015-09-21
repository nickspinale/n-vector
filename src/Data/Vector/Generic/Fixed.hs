{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Vector.Generic.Fixed
    (
    ) where

import qualified Data.Vector.Generic as G
import           GHC.Exts
import           GHC.TypeLits
import           Numeric.Mod

newtype V (n :: Nat) v a = V { unV :: v a }

-- === Conversion ===

toFixed :: forall n v a. (KnownNat n, G.Vector v a) => a -> v a -> V n v a
toFixed padding v = let m = natValInt' (proxy# :: Proxy# n)
                        l = G.length v
                    in if m > l
                       then V (v G.++ G.replicate (m - l) padding)
                       else V (G.take m v)

fromFixed :: V n v a -> v a
fromFixed = unV

-- === Helpers ===

natValInt' :: KnownNat n => Proxy# n -> Int
natValInt' m = fromInteger (natVal' m)

-- === Replicated vector api ===

-- ACCESSORS

(!) :: (KnownNat n, G.Vector v a) => V n v a -> Mod n -> a
(V v) ! i = v G.! fromIntegral i

slice :: ( KnownNat start
         , KnownNat length
         , KnownNat total
         , GT ~ CmpNat total (start + length)
         , G.Vector v a
         ) => Proxy# start -> Proxy# length -> V total v a -> V length v a

slice n m = V . G.slice (natValInt' n) (natValInt' m) . unV

-- CONSTRUCTION

empty :: G.Vector v a => V 0 v a
empty = V G.empty

singleton :: G.Vector v a => a -> V 1 v a
singleton = V . G.singleton

replicate :: (KnownNat n, G.Vector v a) => Proxy# n -> a -> V n v a
replicate p a = V (G.replicate (natValInt' p) a)

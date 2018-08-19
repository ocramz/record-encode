{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
-- {-# language DeriveGeneric #-}

{-|
This library provides generic machinery (via 'GHC.Generics') to encode values of some algebraic type as points in a corresponding Euclidean vector space.

Analyzing datasets that have one or more categorical variables (== values having a sum type) typically requires a series of boilerplate transformations, and the 'encodeOneHot' function provided here does precisely that.


= Usage example

>>> :set -XDeriveGeneric

@
import GHC.Generics
import Data.Record.Encode

data X = A | B | C deriving (Generic)
@

>>> encodeOneHot B
[0,1,0]



== Internals

This library makes use of generic programming to analyze both values and types (see the 'Data.Record.Encode.Generics' module).

Initially, it was relying on Template Haskell to analyze /types/, using the the instance generation machinery explained here: <https://markkarpov.com/tutorial/th.html#example-1-instance-generation>


-}
module Data.Record.Encode (
  -- * One-hot encoding
    encodeOneHot
  -- ** Typeclasses
  , GIndex(..), GVariants(..)
  ) where

import GHC.Generics 
import Data.Proxy

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Data.Record.Encode.Generics

-- data X = A | B | C deriving Generic


-- | Computes the one-hot encoding of a value of a sum type.
--
-- The type of the input value must only be an instance of 'Generic'.
encodeOneHot :: forall a . (GIndex (Rep a), GVariants (Rep a), Generic a) => a -> V.Vector Int
encodeOneHot x = oneHot len i where
  len = fromIntegral $ gnconstructors (Proxy :: Proxy a)
  i = gindex $ from x

-- | Create a one-hot vector
oneHot :: Num a =>
          Int  -- ^ Vector length
       -> Int  -- ^ Index of "1" entry
       -> V.Vector a
oneHot n i = V.create $ do
  vm <- VM.replicate n 0
  VM.write vm i 1
  return vm

-- class Encode i d where
--   -- type ETy d :: *
--   encode :: d -> V.Vector i
--   -- type EIx d :: *  
--   -- encode :: d -> V.Vector (ETy d)
--   -- encodeSparse :: d -> V.Vector (EIx d, EIx d, ETy d)

-- -- | Some pointwise decision (e.g. maximum a posteriori) from a mixture of labels to a single value
-- class Decode i d where
--   decode :: V.Vector i -> d
  
  


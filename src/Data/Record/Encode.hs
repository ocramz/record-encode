{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}

{-# language DeriveGeneric, TemplateHaskell #-}

{-|
This library provides generic machinery to encode values of some algebraic type as points in a corresponding Euclidean vector space.

Analyzing datasets that have one or more categorical variables (== values having a sum type) typically requires a series of boilerplate transformations, and the 'encodeOneHot' function provided here does precisely that.


= Usage example

>>> :set -XDeriveGeneric
>>> :set -XTemplateHaskell 

@
import GHC.Generics
import Data.Record.Encode

data X = A | B | C deriving (Generic)
'deriveCountable' ''X
@

>>> encodeOneHot B
[0,1,0]



== Internals

Template Haskell is used to analyze /types/, whereas "generics" are used to analyze /values/.

* To analyze a type we'll use the instance generation machinery explained here:

https://markkarpov.com/tutorial/th.html#example-1-instance-generation

* To analyze a value, we'll require its type to have a GHC.Generics.Generic instance, and then operate on the generic representation.


-}
module Data.Record.Encode (encodeOneHot, GIndex(..), Countable(..), deriveCountable) where

import qualified GHC.Generics as G
import Data.Proxy

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Data.Record.Decode.TH
import Data.Record.Encode.Generics

data X = A | B | C deriving G.Generic
deriveCountable ''X


-- | Computes the one-hot encoding of a value of a sum type.
--
-- The type must be an instance of 'Generic' (for computing its nonzero index) and 'Countable' (for computing the number of constructors in the type, via TH).
encodeOneHot :: forall p . (GIndex (G.Rep p), G.Generic p, Countable p) => p -> V.Vector Int
encodeOneHot x = oneHot len i where
  len = fromIntegral $ count (Proxy :: Proxy p)
  i = gindex $ G.from x


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
  
  


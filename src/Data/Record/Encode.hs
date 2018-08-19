{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
module Data.Record.Encode (encodeOneHot, GIndex(..), Countable(..), deriveCountable) where

import qualified GHC.Generics as G
import Data.Proxy

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Data.Record.Decode.TH
import Data.Record.Encode.Generics



{-|

Template Haskell is used to analyze /types/, whereas "generics" are used to analyze /values/.

The above distinction is fundamental; for example,

* To analyze a type we'll use machinery similar to this:

https://markkarpov.com/tutorial/th.html#example-1-instance-generation

* To analyze a value, we'll require its type to have both a GHC.Generics.Generic instance and a Generics.SOP.Generic one (from 'generics-sop'), and then operate on the generic representation.

-}



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
  
  


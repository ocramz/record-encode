{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language DeriveGeneric #-}
{-# language ConstraintKinds #-}

{-|
This library provides generic machinery (via GHC.Generics and `generics-sop`) to encode values of some algebraic type as points in a vector space.

Processing datasets that have one or more categorical variables (which in other words are values of a sum type) typically requires a series of boilerplate transformations, and the 'encodeOneHot' function provided here does precisely that.


== Internals

This library makes use of generic programming to analyze both values and types (see the internal Data.Record.Encode.Generics module).

Initially, it was relying on Template Haskell to analyze /types/, using the the instance generation machinery explained here: <https://markkarpov.com/tutorial/th.html#example-1-instance-generation>


-}
module Data.Record.Encode (
  -- * One-hot encoding
    encodeOneHot
  -- ** Types and Utilities
    , OneHot(..), compareOH, oneHotV
  -- * Generics-related
    , G
  ) where

import qualified GHC.Generics as G
import Generics.SOP hiding (Proxy)
import Data.Proxy

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Data.Record.Encode.Generics



-- data X a = A | B a | C | D | E | F deriving G.Generic
-- instance Generic (X a)

data X = A | B | C deriving (G.Generic)
instance Generic X

-- | Constraints necessary to 'encodeOneHot' a value.
--
-- NB: 'GVariants' is an internal typeclass, and this constraint is automatically satisfied if the type is an instance of 'G.Generic'
type G a = (GVariants (G.Rep a), G.Generic a, Generic a)

-- | Computes the one-hot encoding of a value of a sum type.
--
-- The type of the input value must be an instance of 'GHC.Generics.Generic' (from GHC.Generics) /and/ of 'Generics.SOP.Generic' (from the `generics-sop` library).
--
-- >>> :set -XDeriveGeneric
--
-- >>> import qualified GHC.Generics as G
-- >>> import qualified Generics.SOP as SOP
-- >>> import Data.Record.Encode
--
-- >>> data X = A | B | C deriving (G.Generic)
-- >>> instance SOP.Generic X
--
-- >>> encodeOneHot B
-- OH {oDim = 3, oIx = 1}
encodeOneHot :: forall a . G a => a -> OneHot
encodeOneHot x = OH len i where
  len = fromIntegral $ gnconstructors (Proxy :: Proxy a)
  i = gindex $ from x

-- | Create a one-hot vector
oneHotV :: Num a =>
           OneHot
        -> V.Vector a
oneHotV (OH n i) = V.create $ do
  vm <- VM.replicate n 0
  VM.write vm i 1
  return vm



-- | A one-hot encoding is a d-dimensional vector having a single component equal to 1 and all others equal to 0.
data OneHot = OH {
  oDim :: !Int -- ^ Dimension of ambient space (i.e. number of categories)
  , oIx :: !Int  -- ^ Index of nonzero entry
  } deriving (Eq, Show)

compareOH :: OneHot -> OneHot -> Maybe Ordering
compareOH (OH d1 i1) (OH d2 i2)
  | d1 /= d2 = Nothing
  | otherwise = Just (compare i1 i2)


-- class Encode i d where
--   -- type ETy d :: *
--   encode :: d -> V.Vector i
--   -- type EIx d :: *  
--   -- encode :: d -> V.Vector (ETy d)
--   -- encodeSparse :: d -> V.Vector (EIx d, EIx d, ETy d)

-- -- | Some pointwise decision (e.g. maximum a posteriori) from a mixture of labels to a single value
-- class Decode i d where
--   decode :: V.Vector i -> d
  
  


{- |

from A
  :: (C1 _ U1 :+: (C1 _ U1 +: C1 _ U1)) :+: (C1 _ U1 :+: (C1 _ U1 :+: C1 _ U1))

-}

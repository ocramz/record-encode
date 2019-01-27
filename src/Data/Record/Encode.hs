{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language DeriveGeneric, DeriveDataTypeable #-}
{-# language ConstraintKinds #-}
{-# language DataKinds, GADTs, RankNTypes #-}

{-|
This library provides generic machinery (via GHC.Generics and `generics-sop`) to encode values of some algebraic type as points in a vector space.

Processing datasets that have one or more categorical variables (which in other words are values of a sum type) typically requires a series of boilerplate transformations, and the 'encodeOneHot' function provided here does precisely that.


== Internals

This library makes use of generic programming to analyze both values and types (see the 'Data.Record.Encode.Generics' module).


-}
module Data.Record.Encode (
  -- * One-hot encoding
    encodeOneHot, encodeOneHotData
  -- ** Types 
    , OneHot(..)
  -- ** Utilities  
    , compareOH, oneHotV
  -- * Generics-related
    , G
  ) where

import qualified GHC.Generics as G
import Generics.SOP hiding (Proxy)
import Data.Proxy
import Data.Data (Data(..), DataType, Constr, isAlgType, dataTypeConstrs, indexConstr, constrFields, constrIndex, constrType, maxConstrIndex, readConstr, fromConstr, fromConstrB, fromConstrM)
import Data.Typeable
import Data.Maybe

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Data.Record.Encode.Generics

-- $setup
-- >>> :set -XDeriveGeneric
-- >>> :set -XDeriveDataTypeable
-- >>> import Data.Data
-- >>> import qualified GHC.Generics as G
-- >>> import qualified Generics.SOP as SOP
-- >>> import Data.Record.Encode
-- >>> data X = A | B | C deriving (Enum, Data, G.Generic)
-- >>> instance SOP.Generic X


-- | Constraints necessary to 'encodeOneHot' a value.
--
-- NB: 'GVariants' is an internal typeclass, and this constraint is automatically satisfied if the type is an instance of 'G.Generic'
type G a = (GVariants (G.Rep a), G.Generic a, Generic a)

-- | Computes the one-hot encoding of a value of a sum type. A sum type is defined as a choice between N type constructors, each having zero or more fields.
--
-- The number of constructors becomes the dimensionality of the embedding space, and the constructor position (as defined in its implementation) is interpreted as the index of the nonzero coordinate.
--
-- NB : This function computes the generic representation /only/ up to the /outermost/ constructor (see examples below).
--
-- The type of the input value must be an instance of 'GHC.Generics.Generic' (from GHC.Generics) /and/ of 'Generics.SOP.Generic' (from the `generics-sop` library).
--
-- @
-- > :set -XDeriveGeneric
-- 
-- > import qualified GHC.Generics as G
-- > import qualified Generics.SOP as SOP
-- > import Data.Record.Encode
-- 
-- > data X = A | B | C deriving (Enum, G.Generic)
-- > instance SOP.Generic X
-- @
--
-- The @B@ constructor is the second (i.e. position 1 counting from 0) of a choice of three :
-- 
-- >>> encodeOneHot B
-- OH {oDim = 3, oIx = 1}
--
-- The @Just@ constructor is the second of a choice of two:
--
-- >>> encodeOneHot $ Just B
-- OH {oDim = 2, oIx = 1}
--
-- The @Nothing@ constructor is the first:
-- 
-- >>> encodeOneHot (Nothing :: Maybe Int)
-- OH {oDim = 2, oIx = 0}
encodeOneHot :: forall a . G a => a -> OneHot
encodeOneHot x = OH len i where
  len = fromIntegral $ gnconstructors (Proxy :: Proxy a)
  i = gindex $ from x

-- | 'encodeOneHot', requiring only 'Data.Data' instances.
--
-- >>> encodeOneHotData B
-- OH {oDim = 3, oIx = 1}
encodeOneHotData :: Data a => a -> OneHot
encodeOneHotData x = OH len i where
  len = length $ dataTypeConstrs $ dataTypeOf x
  i = constrIndex (toConstr x) - 1

-- | Create a one-hot vector
oneHotV :: Num a =>
           OneHot
        -> V.Vector a
oneHotV (OH n i) = V.create $ do
  vm <- VM.replicate n 0
  VM.write vm i 1
  return vm



-- | A one-hot encoding is a d-dimensional vector having a single component equal to 1 and all others equal to 0.
-- We represent it here compactly as two integers: an integer dimension and an index (which must both be nonnegative).
data OneHot = OH {
  oDim :: !Int -- ^ Dimension of embedding space (i.e. number of categories)
  , oIx :: !Int  -- ^ Index of nonzero coordinate
  } deriving (Eq, Show)

-- | Compares two one-hot encodings for equality. Returns Nothing if the operand dimensions are not equal.
--
-- >>> compareOH (OH 3 2) (OH 3 1)
-- Just GT
--
-- >>> compareOH (OH 3 2) (OH 5 1)
-- Nothing
compareOH :: OneHot -> OneHot -> Maybe Ordering
compareOH (OH d1 i1) (OH d2 i2)
  | d1 /= d2 = Nothing
  | otherwise = Just (compare i1 i2)








data T = T1 | T2 | T3 deriving (Eq, Show, G.Generic)
instance Generic T
instance HasDatatypeInfo T
data A = A { a1 :: T, a2 :: Either Int String } deriving (Eq, Show, G.Generic)
instance Generic A
instance HasDatatypeInfo A
data B = B A (Maybe T) deriving (Eq, Show, G.Generic)
instance Generic B
instance HasDatatypeInfo B
data R = R Int Char deriving (Eq, Show)
data R0 = R0 Int deriving (Eq, Show)
newtype R1 = R1 Int deriving (Eq, Show)

a0 :: A
a0 = A T2 (Right "moo")

b0 = B a0 (Just T3)


gToNP :: (Generic a, Code a ~ '[x]) => a -> NP I x
gToNP d = unZ $ unSOP (from d)






-- class Encode i d where
--   -- type ETy d :: *
--   encode :: d -> V.Vector i
--   -- type EIx d :: *  
--   -- encode :: d -> V.Vector (ETy d)
--   -- encodeSparse :: d -> V.Vector (EIx d, EIx d, ETy d)

-- -- | Some pointwise decision (e.g. maximum a posteriori) from a mixture of labels to a single value
-- class Decode i d where
--   decode :: V.Vector i -> d
  
  





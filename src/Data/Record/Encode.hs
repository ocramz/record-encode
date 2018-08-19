{-# language DeriveGeneric #-}
{-# language GADTs #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
{-# language MultiParamTypeClasses #-}
{-# language QuasiQuotes #-}

{-# language TemplateHaskell #-}

{-# language ScopedTypeVariables #-}
-- {-# LANGUAGE BangPatterns, RankNTypes #-}
module Data.Record.Encode where

import qualified GHC.Generics as G

import Generics.SOP
import Generics.SOP.NP
import Generics.SOP.NS
import Generics.SOP.Constraint -- (SListIN(..))

import GHC.TypeNats

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- import GHC.ST
-- import Control.Monad.Primitive
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


-- λ> from (42, 'z')
-- SOP (Z (I 42 :* I 'z' :* Nil))

-- -- λ> from (A 42)
-- -- SOP (Z (I 42 :* Nil))
-- -- λ> from B
-- -- SOP (S (Z Nil))
-- -- λ> from C
-- -- SOP (S (S (Z Nil)))
-- data X = A Int | B | C deriving (Eq, Show, G.Generic)
-- instance Generic X 





-- | 'gfromEnum' returns the index of each constructor, encoded as an integer.
-- λ> gfromEnum $ P2 Ax Bx
-- [0,1]
-- λ> gfromEnum $ P2 Ax Cx
-- [0,2]
gfromEnum :: (AllF (All Enum) (Code a), Generic a) => a -> [Int]
gfromEnum = hcollapse . hcmap (Proxy :: Proxy Enum) (mapIK fromEnum) . from

-- | !!! Enum types can only have nullary (== 0-argument) constructors
-- data Fz a = Az a | Bz deriving (Eq, Show, Enum, G.Generic)

-- collapse_NP :: NP (K a) xs -> [a]

-- f ~> g :: forall a . f a -> g a 


-- mkOH :: (GIndex f, Countable (f p)) => f p -> V.Vector Integer
-- mkOH x = oneHot l i 
--   where
--     i = gindex x
--     l = fromIntegral $ count (Proxy :: Proxy p) -- (Proxy :: Proxy Int)

-- lenp :: Countable p => p -> Int
-- lenp _ = fromIntegral $ count (Proxy :: Proxy p)


data X = Xa | Xb | Xc deriving (Eq, Show, G.Generic)
deriveCountable ''X

mkOH :: forall p . (GIndex (G.Rep p), G.Generic p, Countable p) => p -> V.Vector Int
mkOH x = oneHot len i where
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
  
  

unZSOP :: SOP f '[x] -> NP f x
unZSOP = unZ . unSOP



-- * Experiments with 'hcmap'.
-- Constrained folds over types that have a Generic representation
--
-- The constraint in the function argument of 'hcmap' indicates
-- that all elements should share some common property, e.g. being instances
-- of some class. See the role of Bla and Show.
--
-- The Proxy argument hints the compiler as to which type to use (in this case, which typeclass to serve as constraint of all elements)

class Bla a where
  bla :: a -> ()
  bla _ = ()


-- instance Bla Fx 

-- instance Bla (Fy a) 

-- -- instance Bla (P1 a) 

-- gbla :: (AllF (All Bla) (Code a), Generic a) => a -> [()]
-- gbla = hcollapse . hcmap (Proxy :: Proxy Bla) (mapIK bla) . from

-- gshow :: (AllF (All Show) (Code a), Generic a) => a -> [String]
-- gshow = hcollapse . hcmap (Proxy :: Proxy Show) (mapIK show) . from


--



data P0 = P0 Bool Char deriving (Eq, Show, G.Generic)
instance Generic P0

deriveCountable ''Bool
deriveCountable ''Char
-- deriveCountable ''Integer
deriveCountable ''P0


-- λ> hcmap (Proxy :: Proxy Show) (mapIK (const ())) $ from $ P0 42 'z'
-- SOP (Z (K () :* K () :* Nil))


data Fx = Ax | Bx | Cx deriving (Eq, Show, Enum, G.Generic)
instance Generic Fx

data Fy a = Ay a | By | Cy deriving (Eq, Show, G.Generic)
instance Generic (Fy a)

data Gx = Ax' | Bx' | Cx' deriving (Eq, Show, Enum, G.Generic)
instance Generic Gx

-- | a Product-Of-Sums
data P1 a = P1 Fx (Fy a) deriving (Eq, Show, G.Generic)
instance Generic (P1 a)

p10 :: P1 Integer
p10 = P1 Ax (Ay 42)

gp10 :: SOP I '[ '[Fx, Fy Integer] ]
gp10 = from p10

-- λ> from $ P1 Ax (Ay 42)
-- SOP (Z (I Ax :* I (Ay 42) :* Nil))


data P2 = P2 Fx Fx deriving (Eq, Show, G.Generic)
instance Generic P2




-- sandbox

moo :: Q [Dec]
moo = [d| mooQ x = x + 1 |]

-- data Moo a = Moo a deriving (Show, G.Generic)
-- moo = [| Moo |]


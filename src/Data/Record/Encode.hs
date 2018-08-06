{-# language DeriveGeneric #-}
{-# language GADTs #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
-- {-# LANGUAGE BangPatterns, RankNTypes #-}
module Data.Record.Encode where

import qualified GHC.Generics as G

import Generics.SOP
import Generics.SOP.NP
import Generics.SOP.NS
import Generics.SOP.Constraint -- (SListIN(..))

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (dataToExpQ)

import Data.Data
import Data.Typeable (Typeable(..), cast)

-- import GHC.Generics

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


data P0 = P0 Int Char deriving (Eq, Show, G.Generic)
instance Generic P0

-- λ> hcmap (Proxy :: Proxy Show) (mapIK (const ())) $ from $ P0 42 'z'
-- SOP (Z (K () :* K () :* Nil))


data Fx = Ax | Bx | Cx deriving (Eq, Show, Enum, G.Generic)
instance Generic Fx

data Fy a = Ay a | By | Cy deriving (Eq, Show, G.Generic)
instance Generic (Fy a)

-- | a Product-Of-Sums
data P1 a = P1 Fx (Fy a) deriving (Eq, Show, G.Generic)
instance Generic (P1 a)

-- λ> from $ P1 Ax (Ay 42)
-- SOP (Z (I Ax :* I (Ay 42) :* Nil))


data P2 = P2 Fx Fx deriving (Eq, Show, G.Generic)
instance Generic P2

-- | Can be used to encode
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

unZSOP :: SOP f '[x] -> NP f x
unZSOP = unZ . unSOP



-- * Experiments with 'hcmap'+'hcollapse'.
-- Constrained folds over types that have a Generic representation
--
-- The constraint in the function argument of 'hcmap' indicates
-- that all elements should share some common property, e.g. being instances
-- of some class. See the role of Bla and Show.
--
-- The Proxy argument hints the compiler to use the

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



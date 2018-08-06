{-# language DeriveGeneric #-}
module Data.Record.Encode where

import qualified GHC.Generics as G

import Generics.SOP
import Generics.SOP.NP
import Generics.SOP.NS
import Generics.SOP.Constraint (SListIN(..))

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


data Fx = Ax | Bx | Cx deriving (Eq, Show, G.Generic)
instance Generic Fx

data Fy a = Ay a | By | Cy deriving (Eq, Show, G.Generic)
instance Generic (Fy a)

-- | a Product-Of-Sums
data P1 a = P1 Fx (Fy a) deriving (Eq, Show, G.Generic)
instance Generic (P1 a)

-- λ> from $ P1 Ax (Ay 42)
-- SOP (Z (I Ax :* I (Ay 42) :* Nil))

{-# language DeriveGeneric #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language MultiParamTypeClasses #-}

{-# language GADTs, DataKinds, PolyKinds, FlexibleInstances, FlexibleContexts #-}
module Data.Record.Encode.Generics where

import GHC.Generics

import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG


-- | Compute the structural index of a value of a sum type via its Generic representation
-- e.g.
-- 
-- λ> gindex $ from Sb
-- 1
-- 
-- λ> gindex $ from Sc
-- 2

class GIndex f where
  gindex :: f p -> Int

instance GIndex (K1 a k) where
  gindex _ = 0

instance (GIndex l, GIndex r) => GIndex (l :+: r) where
  gindex x = case x of
    L1 l -> gindex l
    R1 r -> 1 + gindex r

instance GIndex _i => GIndex (M1 _a _b _i) where
  gindex (M1 x) = gindex x

instance GIndex U1 where
  gindex _ = 0


-- | toy data
data S0 = Sa | Sb | Sc deriving (Eq, Show, Generic)  













-- | 

class GEncode i o where
  gencode :: i x -> Maybe (o x)

-- λ> :t fmap to . gencode . from
-- 
-- fmap to . gencode . from
--   :: (GEncode (Rep a) (Rep b), Generic b, Generic a) =>
--      a -> Maybe b


-- instance GEncode (V1 p) where
--   gencode _ = error "Cannot encode V1"

-- instance GEncode (U1 p) where
--   gencode U1 = error "Cannot encode U1"






data OneHot = OH !Int !Int deriving (Eq, Show) 

class Encode d where
  type ETy d :: *
  type ETy d = OneHot -- (Int, Int)
  -- encode :: d -> V.Vector (ETy d)
  encode :: d x -> ETy d

-- instance Encode


  



-- instance Encode (V1 p) where
--   encode _ = error "Cannot encode V1"

-- instance Encode (U1 p) where
--   encode U1 = error "Cannot encode U1"

-- instance Encode (K1 i c p) where

-- instance Encode (M1 i c f p) where

-- instance Encode ((f :*: g) p) where

-- instance Encode ((f :+: g) p) where  
  




-- class VG.Vector v x => GEncode' i v x where
--   gencode' :: i x -> Maybe (v Int)

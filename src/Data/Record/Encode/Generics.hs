{-# language DeriveGeneric #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language MultiParamTypeClasses #-}

{-# language GADTs, DataKinds, PolyKinds, FlexibleInstances, FlexibleContexts #-}
module Data.Record.Encode.Generics where

import GHC.Generics

import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG



-- data Rec :: (u -> *) -> [u] -> * where
--   RNil :: Rec f '[]
--   (:&) :: !(f r) -> !(Rec f rs) -> Rec f (r ': rs)

-- instance Generic (Rec f '[]) where
--   type Rep (Rec f '[]) = U1
--   from RNil = U1
--   to U1 = RNil
  
-- instance (Generic (Rec f rs), Rep (Rec f rs) ~ Rec0 (Rec f rs)) => Generic (Rec f (r ': rs)) where
--   type Rep (Rec f (r ': rs)) = Rec0 (f r) :*: Rec0 (Rec f rs)
--   from (x :& xs) = K1 x :*: from xs
--   to (K1 x :*: xs) = x :& to xs






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
  encode :: d -> ETy d

-- instance Encode

-- encode_ xss = go xss 0 where
--   go (x:xs) i 
  



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

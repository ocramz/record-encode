module Data.Record.Encode.Vinyl where

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

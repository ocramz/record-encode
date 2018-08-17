{-# language DeriveGeneric #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
module Data.Record.Encode.Generics where

import GHC.Generics

import qualified Data.Vector as V


class Encode d where
  type ETy d :: *
  encode :: d -> V.Vector (ETy d)

instance Encode (V1 p) where
  encode _ = error "Cannot encode V1"

instance Encode (U1 p) where
  encode U1 = error "Cannot encode U1"

instance Encode (K1 i c p) where

instance Encode (M1 i c f p) where

instance Encode ((f :*: g) p) where

instance Encode ((f :+: g) p) where    


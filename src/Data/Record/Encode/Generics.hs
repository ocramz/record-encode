{-# language TypeOperators #-}
{-# language DeriveGeneric #-}
-- {-# language GADTs, DataKinds, PolyKinds, FlexibleInstances, FlexibleContexts #-}
{-# language KindSignatures, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
module Data.Record.Encode.Generics (
    gindex
  , GVariants(..)
  , gnconstructors
  ) where

import qualified GHC.Generics as G
import Data.Proxy

import Generics.SOP hiding (Proxy)
import Generics.SOP.NS

-- | Compute the structural index of a value of a sum type via its Generic representation
-- e.g.:
-- 
-- >>> data S = Sa | Sb | Sc deriving (Eq, Show, G.Generic)
-- >>> instance Generic S
-- 
-- >>> gindex $ from Sb
-- 1
gindex :: SOP f xs -> Int
gindex = index_NS . unSOP 


-- | Counts the number of outermost constructors ("variants" of a type)
class GVariants (f :: * -> *) where
  vars :: proxy f -> Int

instance GVariants (G.M1 G.C m f) where { vars _ = 1 }

instance GVariants G.V1 where { vars _ = 0 }

instance GVariants f => GVariants (G.M1 G.D m f) where { vars _ = vars (Proxy :: Proxy f) }

instance (GVariants f, GVariants g) => GVariants (f G.:+: g) where { vars _ = vars (Proxy :: Proxy f) + vars (Proxy :: Proxy g) }

-- | Counts the number of outermost constructors
gnconstructors :: forall a . (G.Generic a, GVariants (G.Rep a)) => Proxy a -> Int
gnconstructors _ = vars (Proxy :: Proxy (G.Rep a))


-- [17:21] <mniip> @let class Variants f where { variants :: proxy f -> Int }; instance Variants (M1 C m f) where { variants _ = 1 }; instance Variants V1 where { variants _ = 0 }; instance Variants f => Variants (M1 D m f) where { variants _ = variants (Proxy :: Proxy f) }; instance (Variants f, Variants g) => Variants (f :+: g) where { variants _ = variants (Proxy :: Proxy f) + variants (Proxy :: Proxy g) }
-- [17:21] <lambdabot>  Defined.
-- [17:21] <mniip> @let variants' :: forall a. (Generic a, Variants (Rep a)) => Proxy a -> Int; variants' _ = variants (Proxy :: Proxy (Rep a))
-- [17:21] <lambdabot>  Defined.
-- [17:21] <mniip> > variants' (Proxy :: Proxy Bool)
-- [17:21] <lambdabot>  2
-- [17:22] <mniip> > variants' (Proxy :: Proxy (Maybe (Either Bool Bool)))
-- [17:22] <lambdabot>  2
-- [17:22] <mniip> as you can see this only focuses on the outer type constructor
-- [17:22] <mniip> > variants (Proxy :: Proxy Proxy)















-- -- | 

-- class GEncode i o where
--   gencode :: i x -> Maybe (o x)

-- -- λ> :t fmap to . gencode . from
-- -- 
-- -- fmap to . gencode . from
-- --   :: (GEncode (Rep a) (Rep b), Generic b, Generic a) =>
-- --      a -> Maybe b


-- -- instance GEncode (V1 p) where
-- --   gencode _ = error "Cannot encode V1"

-- -- instance GEncode (U1 p) where
-- --   gencode U1 = error "Cannot encode U1"






-- data OneHot = OH !Int !Int deriving (Eq, Show) 

-- class Encode d where
--   type ETy d :: *
--   type ETy d = OneHot -- (Int, Int)
--   -- encode :: d -> V.Vector (ETy d)
--   encode :: d x -> ETy d

-- -- instance Encode


  



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

{-# language DeriveGeneric #-}
module LibSpec where

import Test.Hspec
-- import Test.Hspec.QuickCheck

import qualified GHC.Generics as G
import Generics.SOP 

import qualified Data.Vector as V

import Data.Record.Encode


data X = Xa | Xb | Xc deriving (Eq, Show, G.Generic)
instance Generic X




main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Data.Record.Encode" $ do
    it "creates a one-hot encoded vector from a sum type" $ do
      V.length (encodeOneHot Xb) `shouldBe` 3
      
      
    -- prop "ourAdd is commutative" $ \x y ->
    --   ourAdd x y `shouldBe` ourAdd y x




-- data P0 = P0 Bool Char deriving (Eq, Show, G.Generic)
-- -- instance Generic P0

-- deriveCountable ''Bool
-- deriveCountable ''Char
-- -- deriveCountable ''Integer
-- deriveCountable ''P0


-- -- λ> hcmap (Proxy :: Proxy Show) (mapIK (const ())) $ from $ P0 42 'z'
-- -- SOP (Z (K () :* K () :* Nil))


-- data Fx = Ax | Bx | Cx deriving (Eq, Show, Enum, G.Generic)
-- -- instance Generic Fx

-- data Fy a = Ay a | By | Cy deriving (Eq, Show, G.Generic)
-- -- instance Generic (Fy a)

-- data Gx = Ax' | Bx' | Cx' deriving (Eq, Show, Enum, G.Generic)
-- -- instance Generic Gx

-- -- | a Product-Of-Sums
-- data P1 a = P1 Fx (Fy a) deriving (Eq, Show, G.Generic)
-- -- instance Generic (P1 a)

-- p10 :: P1 Integer
-- p10 = P1 Ax (Ay 42)

-- -- gp10 :: SOP I '[ '[Fx, Fy Integer] ]
-- -- gp10 = from p10

-- -- λ> from $ P1 Ax (Ay 42)
-- -- SOP (Z (I Ax :* I (Ay 42) :* Nil))


-- data P2 = P2 Fx Fx deriving (Eq, Show, G.Generic)
-- -- instance Generic P2



-- -- data Y = Y (X, X) deriving (Eq, Show, G.Generic)
-- -- deriveCountable ''Y

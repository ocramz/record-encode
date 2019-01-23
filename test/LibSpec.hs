{-# language DeriveGeneric #-}
module LibSpec where

import Test.Hspec
-- import Test.Hspec.QuickCheck
import qualified GHC.Generics as G
import Generics.SOP 

import Data.Record.Encode


data X = Xa | Xb | Xc deriving (Eq, Show, Enum, G.Generic)
instance Generic X


main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Data.Record.Encode" $ do
    it "creates a one-hot encoded vector from a sum type" $ do
      oDim (encodeOneHot Xb) `shouldBe` 3
      oIx (encodeOneHot Xb) `shouldBe` 1
      
    -- prop "ourAdd is commutative" $ \x y ->
    --   ourAdd x y `shouldBe` ourAdd y x



{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
module Data.Record.Encode.Internal where

import Generics.SOP
import qualified GHC.Generics as G

import qualified Data.GenericTrie as T


type Name = String 

data Exp =
    Exp Name [Exp]
  | IntC Int 
  deriving (Eq, Show)

class ToExp a where
    toExp :: a -> Exp


-- -- | >>> prettyExpr $ sopToExpr (gdatatypeInfo (Proxy :: Proxy String)) (gfrom "foo")
-- -- _:_ 'f' "oo"
-- sopToExpr :: (All2 ToExpr xss) => DatatypeInfo xss -> SOP I xss -> Expr
-- sopToExpr di (SOP xss) = hcollapse $ hcliftA2
--     (Proxy :: Proxy (All ToExpr))
--     (\ci xs -> K (sopNPToExpr isNewtype ci xs))
--     (constructorInfo di)
--     xss
--   where
--     isNewtype = case di of
--         Newtype _ _ _ -> True
--         ADT _ _ _     -> False

sopNPToExpr :: All ToExp xs => Bool -> ConstructorInfo xs -> NP I xs -> Exp
sopNPToExpr _ (Infix cn _ _) xs = Exp ("_" ++ cn ++ "_") $ hcollapse $
    hcmap (Proxy :: Proxy ToExp) (mapIK toExp) xs
-- sopNPToExpr _ (Constructor cn) xs = App cn $ hcollapse $
--     hcmap (Proxy :: Proxy ToExpr) (mapIK toExpr) xs
-- sopNPToExpr True (Record cn _) xs = App cn $ hcollapse $
--     hcmap (Proxy :: Proxy ToExpr) (mapIK toExpr) xs
-- sopNPToExpr False (Record cn fi) xs = Rec cn $ T.fromList $ hcollapse $
--     hcliftA2 (Proxy :: Proxy ToExpr) mk fi xs
--   where
--     mk :: ToExpr x => FieldInfo x -> I x -> K (FieldName, Expr) x
--     mk (FieldInfo fn) (I x) = K (fn, toExpr x)


baz :: (All ToExp xs) => NP I xs -> Exp
baz xs = Exp "" $ hcollapse $ hcmap (Proxy :: Proxy ToExp) (mapIK toExp) xs


instance ToExp Int where toExp = IntC
instance ToExp a => ToExp (Maybe a) where
  toExp m = case m of
    Nothing -> Exp "Nothing" []
    Just x  -> Exp "Just" [toExp x]
instance (ToExp a, ToExp b) => ToExp (Either a b) where
  toExp e = case e of
    (Left l)  -> Exp "Left" [toExp l]
    (Right r) -> Exp "Right" [toExp r]
instance (ToExp a, ToExp b) => ToExp (a, b) where
  toExp (x, y) = Exp "(,)" [toExp x, toExp y]

-- v0 = Just 42 :: Maybe Int

v1 = (42, Just 43) :: (Int, Maybe Int)

{-# language TemplateHaskell #-}
module Data.Record.Decode.TH where

import Language.Haskell.TH -- (Q(..), Info(..), Name(..), reify, lookupTypeName, runQ)
import Language.Haskell.TH.Syntax -- (Dec(..), Type(..), dataToExpQ, Quasi(..))

import Data.Data
import Data.Typeable (Typeable(..), cast)

-- | Count the number of distinct values that a type can have
class Countable a where
  count :: Proxy a -> Integer

-- | " supposing the type is Enum and Bounded
deriveCountableSimple :: Name -> Q [Dec]
deriveCountableSimple name = [d|
  instance Countable $a where
    count Proxy = fromIntegral $
      1 + fromEnum (maxBound :: $a) - fromEnum (minBound :: $a)
  |]
  where
    a = conT name

-- | " not supposing any instance 
deriveCountableComposite :: Name -> Q [Dec]
deriveCountableComposite name = do
  TyConI (DataD _ _ _ _ cons' _) <- reify name
  [d|
     instance Countable $(conT name) where
       count Proxy = $(foldr addE [| 0 |] $ f <$> cons')
   |]
  where
    f (NormalC _ ts) = handleCon (snd <$> ts)
    f (RecC    _ ts) = handleCon (thd <$> ts)
    f _              = fail "unsupported data type"
    handleCon ts = foldr mulE [| 1 |] (countTypeE <$> ts)
    countTypeE t = [| count (Proxy :: Proxy $(return t)) |]
    addE x y     = [| $x + $y |]
    mulE x y     = [| $x * $y |]
    thd (_,_,x)  = x

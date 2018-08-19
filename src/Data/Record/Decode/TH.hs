{-# language TemplateHaskell #-}
module Data.Record.Decode.TH (Countable(..), deriveCountable) where

import Control.Monad (foldM)

import Language.Haskell.TH -- (Q(..), Info(..), Name(..), reify, lookupTypeName, runQ)
import Language.Haskell.TH.Syntax -- (Dec(..), Type(..), dataToExpQ, Quasi(..))
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Data.Data
-- import Data.Typeable (Typeable(..), cast)



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
       count Proxy = $(foldr addE [| 0 |] $ unpackCon <$> cons')
   |]
  where
    addE x y     = [| $x + $y |]

unpackCon :: Con -> Q Exp
unpackCon (NormalC _ ts) = handleCon (snd <$> ts)
unpackCon (RecC    _ ts) = handleCon (thd <$> ts)
  where
    thd (_,_,x)  = x
unpackCon _ = fail "unsupported data type"


handleCon :: (Foldable t, Functor t) => t Type -> Q Exp
handleCon ts = foldr mulE [| 1 |] (countTypeE <$> ts) where
    countTypeE t = [| count (Proxy :: Proxy $(return t)) |]
    mulE x y     = [| $x * $y |]



deriveCountable :: Name -> Q [Dec]
deriveCountable name = do
  let ts = [ConT name]
  hasEnum    <- isInstance ''Enum    ts
  hasBounded <- isInstance ''Bounded ts
  if hasEnum && hasBounded
    then deriveCountableSimple    name
    else deriveCountableComposite name






-- | experiments with quasiquotation

dataQ :: Data a => a -> Q Exp
dataQ = dataToExpQ cast

bla :: QuasiQuoter
bla = QuasiQuoter {
  quoteExp = \str -> dataQ str -- undefined
  , quotePat = error "Usage as a pattern is not supported"
  , quoteType = error "Usage as a type is not supported"
  , quoteDec  = error "Usage as a declaration is not supported"  
                  }

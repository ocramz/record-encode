# record-encode

[![Build Status](https://travis-ci.org/ocramz/record-encode.png)](https://travis-ci.org/ocramz/record-encode)

This library provides generic machinery to encode values of some algebraic type as points in a vector space.

Analyzing datasets that have one or more categorical variables (that is, values having a sum type) typically requires a series of boilerplate transformations, and the `encodeOneHot` function provided here does precisely that.

# Usage example

```
    {-# language DeriveGeneric -#}

    import qualified GHC.Generics as G
    import qualified Generics.SOP as SOP
    
    import Data.Record.Encode

    data X = A | B | C deriving (G.Generic)
    instance SOP.Generic X
```

```
    > encodeOneHot B
    [0,1,0]
```



# Acknowledgements

Gagandeep Bhatia (@gagandeepb) for his GSoC '18 work on `Frames-beam`, Mark Karpov (@mrkkrp) for his Template Haskell tutorial, Anthony Cowley (@acowley) for `vinyl` and `Frames`, @mniip on Freenode #haskell for helping me better understand what can be done with generic programming.
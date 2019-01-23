# record-encode

## Encoding categorical variables

[![Build Status](https://travis-ci.org/ocramz/record-encode.png)](https://travis-ci.org/ocramz/record-encode)
[![Hackage](https://img.shields.io/hackage/v/record-encode.svg)](https://hackage.haskell.org/package/record-encode)

This library provides generic machinery to encode values of some algebraic type as points in a vector space.

Values of a sum type (e.g. enumerations) are also called "categorical" variables in statistics, because they encode a choice between a number of discrete categories.

On the other hand, many algorithms rely on a purely numerical representation of data; the conversion code from valyes of a static type is often "boilerplate".

Analyzing datasets that have a number of such categorical variables (that is, values having a sum type) requires mostly identical code for each; the `encodeOneHot` function provided here is a generic way (i.e. defined once and for all) to compute the one-hot representation of any sum type. 

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
    OH {oDim = 3, oIx = 1}
```

Please refer to the documentation of Data.Record.Encode for more examples and details.


# Acknowledgements

Gagandeep Bhatia (@gagandeepb) for his Google Summer of Code 2018 work on [`Frames-beam`](https://github.com/gagandeepb/Frames-beam), Mark Karpov (@mrkkrp) for his Template Haskell tutorial, Anthony Cowley (@acowley) for [`Frames`](https://hackage.haskell.org/package/Frames), @mniip on Freenode #haskell for helping me better understand what can be done with generic programming.
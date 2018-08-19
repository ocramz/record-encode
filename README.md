# record-encode

[![Build Status](https://travis-ci.org/ocramz/record-encode.png)](https://travis-ci.org/ocramz/record-encode)

This library provides generic machinery to encode values of some algebraic type as points in a corresponding Euclidean vector space.

Analyzing datasets that have one or more categorical variables (== values having a sum type) typically requires a series of boilerplate transformations, and the `encodeOneHot` function provided here does precisely that.

# Usage example

```
    {-# language DeriveGeneric -#}

    import GHC.Generics
    import Data.Record.Encode

    data X = A | B | C deriving (Generic)
```

```
    > encodeOneHot B
    [0,1,0]
```



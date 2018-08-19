# record-encode

[![Build Status](https://travis-ci.org/ocramz/record-encode.png)](https://travis-ci.org/ocramz/record-encode)

This library provides generic machinery to encode values of some algebraic type as points in a corresponding Euclidean vector space.

Analyzing datasets that have one or more categorical variables (== values having a sum type) typically requires a series of boilerplate transformations, and the 'encodeOneHot' function provided here addresses precisely that.

# Usage example

    {-# language DeriveGeneric, TemplateHaskell -#}

    import GHC.Generics
    import Data.Record.Encode

    data X = A | B | C deriving (Generic)
    'deriveCountable' ''X


    > encodeOneHot B
    [0,1,0]



## Internals

Template Haskell is used to analyze /types/, whereas "generics" are used to analyze /values/.

* To analyze a type we'll use the instance generation machinery explained here:

https://markkarpov.com/tutorial/th.html#example-1-instance-generation

* To analyze a value, we'll require its type to have a GHC.Generics.Generic instance, and then operate on the generic representation.
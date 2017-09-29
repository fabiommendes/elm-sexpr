# Elm S-expressions

S-expressions are a useful data structure used in LISP to represent pretty much
everything. If you are like me, you might even want to produce JSON like this:
`["date", 2017, 10, 1]`, which I think is neat, but are a real pain to handle 
with Json.Decode and Json.Encode.

This package provides convenient encoders and decoders for S-expression like
data structures so you do not shy away from them in your API ;)

## Features

* An `sexpr [int, int, int]` encoder and decoder.
* An simple SExpr type that can be easily converted to other types.
* Handles up to 8 arguments in the SExpr.

## Basic usage: decoding

```elm
import Json.Decode exposing (..)
import SExpr.Decode exposing (..)

type alias Date =
    { year : Int
    , month : Int
    , day : Int
    }


-- This converts JSON data such as ["date", 2017, 10, 1] to Elm date values 

dateDecoder = sexpr3 "date" (Date, int, int, int)
```

## Basic usage: encoding

```elm
import Json.Encode exposing (..)
import SExpr.Encode exposing (..)

type alias Date =
    { year : Int
    , month : Int
    , day : Int
    }


-- This converts Date elements to JSON data such as ["date", 2017, 10, 1] 
-- In order to perform this conversion, we need a function that flatten
-- a Date structure into a tuple.

dateEncoder = enc3 ("date", int, int, int) 
    (\date -> (date.year, date.month, date.day))
```

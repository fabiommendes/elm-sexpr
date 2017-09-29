# Elm S-expressions

S-expressions are a useful data structure used in LISP to represent pretty much
everything. If you are like me, you might even want to produce JSON like this:

```json
["date", 2017, 10, 1]
```

which I think is neat, but are a real pain to handle with Json.Decode and 
Json.Encode.

This package provides convenient encoders and decoders for S-expression 
structures so you do not have to shy away from using them in your API ;)

## Features

* Tuple encoder/decoder: `tuple2 (int, int)`.
* S-expression encoder/decoder: `exp2 ["point", int, int]`.
* Handles up to 8 arguments in the S-expression and 9 in the tuple.
* Support for complex decoders for union types.

## Basic usage: decoding

```elm
import Json.Decode exposing (..)
import SExpr.Decode exposing (..)

type alias Date =
    { year : Int
    , month : Int
    , day : Int
    }


-- This creates a decoder that reads JSON data such as ["date", 2017, 10, 1]
-- and convert it to Elm date values 

date = exp3 ("date", int, int, int) Date
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


-- This converts Date elements to JSON S-expressions such as 
-- ["date", 2017, 10, 1]. In order to perform this conversion, we need 
-- a function that flatten a Date object into a tuple.

date = exp3 ("date", int, int, int)
    (\date -> (date.year, date.month, date.day))
```

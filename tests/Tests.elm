module Tests exposing (..)

import Test exposing (..)
import Tests.Decode
import Tests.Encode


all : Test
all =
    describe "elm-form Suite"
        [ Tests.Encode.all
        , Tests.Decode.all
        ]

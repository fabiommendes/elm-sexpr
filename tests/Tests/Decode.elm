module Tests.Decode exposing (all)

import Expect exposing (..)
import Json.Decode exposing (..)
import SExpr.Decode exposing (..)
import Test exposing (..)


type alias Date =
    { year : Int, month : Int, day : Int }


type FooBar
    = Foo Int Int
    | Bar Int
    | Baz


foobar : Decoder FooBar
foobar =
    union
        [ opt2 ( "foo", int, int ) Foo
        , opt1 ( "bar", int ) Bar
        , opt0 "baz" Baz
        ]


date : Decoder Date
date =
    exp3 ( "date", int, int, int ) Date


all : Test
all =
    describe "Test decoder"
        [ test "tuple decoder" <|
            \_ ->
                "[1, true, 2.0]"
                    |> decodeString (tuple3 int bool float)
                    |> Expect.equal (Ok ( 1, True, 2.0 ))
        , test "s-expr date decoder" <|
            \_ ->
                "[\"date\",2017,10,1]"
                    |> decodeString date
                    |> Expect.equal (Ok (Date 2017 10 1))
        , test "composite decoder" <|
            \_ ->
                "[\"bar\", 1]"
                    |> decodeString foobar
                    |> Expect.equal (Ok (Bar 1))
        ]

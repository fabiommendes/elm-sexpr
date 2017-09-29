module Tests.Encode exposing (all)

import Expect exposing (..)
import Fuzz exposing (..)
import Json.Encode as E
import SExpr.Encode exposing (..)
import Test exposing (..)


type alias Date =
    { year : Int, month : Int, day : Int }


date =
    exp3 ( "date", E.int, E.int, E.int ) (\d -> ( d.year, d.month, d.day ))


all : Test
all =
    describe "Encoding arbitrary values"
        [ test "Encode tuples" <|
            \_ ->
                tuple2 E.int E.int ( 1, 2 )
                    |> vToString
                    |> Expect.equal "[1,2]"
        , test "Encode single arg tuple" <|
            \_ ->
                tuple1 E.int 1
                    |> vToString
                    |> Expect.equal "[1]"
        , test "Encode multi arg tuple" <|
            \_ ->
                ( 1, 2, 3, 4, 5, 6, 7, 8 )
                    |> tuple8 E.int E.int E.int E.int E.int E.int E.int E.int
                    |> vToString
                    |> Expect.equal "[1,2,3,4,5,6,7,8]"
        , test "Encode nested tuple" <|
            \_ ->
                ( 1, ( 2, 3 ) )
                    |> tuple2 E.int (tuple2 E.int E.int)
                    |> vToString
                    |> Expect.equal "[1,[2,3]]"
        , test "Encode known value" <|
            \_ ->
                Date 2017 10 1
                    |> date
                    |> E.encode 0
                    |> Expect.equal "[\"date\",2017,10,1]"
        , fuzz3 int int int "Encode random date values" <|
            \yy mm dd ->
                let
                    tail =
                        String.join "," <| List.map toString [ yy, mm, dd ]
                in
                Date yy mm dd
                    |> date
                    |> E.encode 0
                    |> Expect.equal ("[\"date\"," ++ tail ++ "]")
        ]


vToString : E.Value -> String
vToString v =
    E.encode 0 v

module SExpr.Decode
    exposing
        ( exp0
        , exp1
        , exp2
        , exp3
        , exp4
        , exp5
        , exp6
        , exp7
        , exp8
        , opt0
        , opt1
        , opt2
        , opt3
        , opt4
        , opt5
        , opt6
        , opt7
        , opt8
        , tuple1
        , tuple2
        , tuple3
        , tuple4
        , tuple5
        , tuple6
        , tuple7
        , tuple8
        , union
        )

{-| Functions used to decode tuple types encoded as JSON arrays and arbitrary
types encoded as S-expressions.


# Tuple decoders

Tuple decoders match tuples represented as non-homogeneous arrays in JSON.
Simply match the corresponding tuple* function and call it with the decoders
for each position:

    point2d = tuple2 int int

    decodeString point2d "[1, 2]" == Ok (1, 2)

@docs tuple1, tuple2, tuple3, tuple4, tuple5, tuple6, tuple7, tuple8


# S-Expression decoders

Those decoders handle simple S-expressions like `["point", 1, 2]`. In order to
decode this expression, use

    type alias Point2d = {x : Int, y : Int}
    point2d = exp2 ("point", int, int) Point2d

    decodeString point2d "[\"point\", 1, 2]" == Ok Point2d {x = 1, y = 2}

It assumes fixed length S-Expressions and a constructor that takes 2
arguments.

@docs exp0, exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8


# Composite decoders for union types.

Union types might be encoded with a variable header (which might correspond to
a variable tail decoder). Consider the type bellow:

    type Info
        = Pos Int Int -- encoded as ["pos", x, y]
        | Color String -- encoded as ["color", name]

The decoder can be created using:

    info =
        union
            [ opt0 ( "pos", int, int ) Pos
            , opt1 ( "color", string ) Color
            ]

The head indentifier must be unique, and it selects the most apropriate
sub-decoder for a given expression:

    decodeString info "[\"pos\", 0, 1]" == Ok (Pos 0 1)

@docs union, opt0, opt1, opt2, opt3, opt4, opt5, opt6, opt7, opt8

-}

import Dict
import Json.Decode exposing (..)
import SExpr.DecodeTupleAt exposing (..)


--- TUPLE DECODERS -------------------------------------------------------------


{-| Decodes an 1-tuple
-}
tuple1 : Dec a -> Dec a
tuple1 a =
    tuple1At 0 a |> withSize 1


{-| Decodes an 2-tuple
-}
tuple2 : Dec a -> Dec b -> Dec ( a, b )
tuple2 a b =
    tuple2At 0 a b |> withSize 2


{-| Decodes an 3-tuple
-}
tuple3 : Dec a -> Dec b -> Dec c -> Dec ( a, b, c )
tuple3 a b c =
    tuple3At 0 a b c |> withSize 3


{-| Decodes an 4-tuple
-}
tuple4 : Dec a -> Dec b -> Dec c -> Dec d -> Dec ( a, b, c, d )
tuple4 a b c d =
    tuple4At 0 a b c d |> withSize 4


{-| Decodes an 5-tuple
-}
tuple5 : Dec a -> Dec b -> Dec c -> Dec d -> Dec e -> Dec ( a, b, c, d, e )
tuple5 a b c d e =
    tuple5At 0 a b c d e |> withSize 5


{-| Decodes an 6-tuple
-}
tuple6 : Dec a -> Dec b -> Dec c -> Dec d -> Dec e -> Dec f -> Dec ( a, b, c, d, e, f )
tuple6 a b c d e f =
    tuple6At 0 a b c d e f |> withSize 6


{-| Decodes an 7-tuple
-}
tuple7 : Dec a -> Dec b -> Dec c -> Dec d -> Dec e -> Dec f -> Dec g -> Dec ( a, b, c, d, e, f, g )
tuple7 a b c d e f g =
    tuple7At 0 a b c d e f g |> withSize 7


{-| Decodes an 8-tuple
-}
tuple8 : Dec a -> Dec b -> Dec c -> Dec d -> Dec e -> Dec f -> Dec g -> Dec h -> Dec ( a, b, c, d, e, f, g, h )
tuple8 a b c d e f g h =
    tuple8At 0 a b c d e f g h |> withSize 8


{-| Decodes an 9-tuple
-}
tuple9 : Dec a -> Dec b -> Dec c -> Dec d -> Dec e -> Dec f -> Dec g -> Dec h -> Dec i -> Dec ( a, b, c, d, e, f, g, h, i )
tuple9 a b c d e f g h i =
    tuple9At 0 a b c d e f g h i |> withSize 9



--- SIMPLE S-EXPRESSION DECODERS -----------------------------------------------


{-| Decodes an 0 arguments S-expression
-}
exp0 : String -> x -> Dec x
exp0 name cons =
    tuple1 string
        |> andThen
            (\x ->
                if x == name then
                    succeed cons
                else
                    fail <| "unexpected value: " ++ x
            )


{-| Decodes an 1 arguments S-expression
-}
exp1 : ( String, Dec a ) -> (a -> x) -> Dec x
exp1 ( name, dec1 ) cons =
    tuple2 string dec1
        |> andThen
            (\( h, a ) ->
                check name h <| \() -> cons a
            )


{-| Decodes an 2 arguments S-expression
-}
exp2 : ( String, Dec a, Dec b ) -> (a -> b -> x) -> Dec x
exp2 ( name, dec1, dec2 ) cons =
    tuple3 string dec1 dec2
        |> andThen
            (\( h, a, b ) ->
                check name h <| \() -> cons a b
            )


{-| Decodes an 3 arguments S-expression
-}
exp3 : ( String, Dec a, Dec b, Dec c ) -> (a -> b -> c -> x) -> Dec x
exp3 ( name, dec1, dec2, dec3 ) cons =
    tuple4 string dec1 dec2 dec3
        |> andThen
            (\( h, a, b, c ) ->
                check name h <| \() -> cons a b c
            )


{-| Decodes an 4 arguments S-expression
-}
exp4 : ( String, Dec a, Dec b, Dec c, Dec d ) -> (a -> b -> c -> d -> x) -> Dec x
exp4 ( name, dec1, dec2, dec3, dec4 ) cons =
    tuple5 string dec1 dec2 dec3 dec4
        |> andThen
            (\( h, a, b, c, d ) ->
                check name h <| \() -> cons a b c d
            )


{-| Decodes an 5 arguments S-expression
-}
exp5 : ( String, Dec a, Dec b, Dec c, Dec d, Dec e ) -> (a -> b -> c -> d -> e -> x) -> Dec x
exp5 ( name, dec1, dec2, dec3, dec4, dec5 ) cons =
    tuple6 string dec1 dec2 dec3 dec4 dec5
        |> andThen
            (\( h, a, b, c, d, e ) ->
                check name h <| \() -> cons a b c d e
            )


{-| Decodes an 6 arguments S-expression
-}
exp6 : ( String, Dec a, Dec b, Dec c, Dec d, Dec e, Dec f ) -> (a -> b -> c -> d -> e -> f -> x) -> Dec x
exp6 ( name, dec1, dec2, dec3, dec4, dec5, dec6 ) cons =
    tuple7 string dec1 dec2 dec3 dec4 dec5 dec6
        |> andThen
            (\( h, a, b, c, d, e, f ) ->
                check name h <| \() -> cons a b c d e f
            )


{-| Decodes an 7 arguments S-expression
-}
exp7 : ( String, Dec a, Dec b, Dec c, Dec d, Dec e, Dec f, Dec g ) -> (a -> b -> c -> d -> e -> f -> g -> x) -> Dec x
exp7 ( name, dec1, dec2, dec3, dec4, dec5, dec6, dec7 ) cons =
    tuple8 string dec1 dec2 dec3 dec4 dec5 dec6 dec7
        |> andThen
            (\( h, a, b, c, d, e, f, g ) ->
                check name h <| \() -> cons a b c d e f g
            )


{-| Decodes an 8 arguments S-expression
-}
exp8 : ( String, Dec a, Dec b, Dec c, Dec d, Dec e, Dec f, Dec g, Dec h ) -> (a -> b -> c -> d -> e -> f -> g -> h -> x) -> Dec x
exp8 ( name, dec1, dec2, dec3, dec4, dec5, dec6, dec7, dec8 ) cons =
    tuple9 string dec1 dec2 dec3 dec4 dec5 dec6 dec7 dec8
        |> andThen
            (\( head, a, b, c, d, e, f, g, h ) ->
                check name head <| \() -> cons a b c d e f g h
            )



--- UNION DECODERS -------------------------------------------------------------


{-| Declares a S-Expression decoder for a union type
-}
union : List ( String, Decoder a ) -> Decoder a
union lst =
    let
        decoders =
            Dict.fromList lst

        jsonToDec : Value -> Decoder a
        jsonToDec json =
            case decodeValue (index 0 string) json of
                Ok head ->
                    case Dict.get head decoders of
                        Just dec ->
                            dec

                        Nothing ->
                            fail ("invalid decoder: " ++ head)

                Err msg ->
                    fail msg
    in
    value |> andThen jsonToDec


{-| Declares a 0 argument S-Expression state
-}
opt0 : String -> x -> ( String, Dec x )
opt0 name cons =
    ( name, exp0 name cons )


{-| Declares a 1 argument S-Expression state
-}
opt1 : ( String, Dec a ) -> (a -> t) -> ( String, Dec t )
opt1 ( name, a ) cons =
    ( name, exp1 ( name, a ) cons )


{-| Declares a 2 argument S-Expression state
-}
opt2 : ( String, Dec a, Dec b ) -> (a -> b -> t) -> ( String, Dec t )
opt2 ( name, a, b ) cons =
    ( name, exp2 ( name, a, b ) cons )


{-| Declares a 3 argument S-Expression state
-}
opt3 : ( String, Dec a, Dec b, Dec c ) -> (a -> b -> c -> t) -> ( String, Dec t )
opt3 ( name, a, b, c ) cons =
    ( name, exp3 ( name, a, b, c ) cons )


{-| Declares a 4 argument S-Expression state
-}
opt4 : ( String, Dec a, Dec b, Dec c, Dec d ) -> (a -> b -> c -> d -> t) -> ( String, Dec t )
opt4 ( name, a, b, c, d ) cons =
    ( name, exp4 ( name, a, b, c, d ) cons )


{-| Declares a 5 argument S-Expression state
-}
opt5 : ( String, Dec a, Dec b, Dec c, Dec d, Dec e ) -> (a -> b -> c -> d -> e -> t) -> ( String, Dec t )
opt5 ( name, a, b, c, d, e ) cons =
    ( name, exp5 ( name, a, b, c, d, e ) cons )


{-| Declares a 6 argument S-Expression state
-}
opt6 : ( String, Dec a, Dec b, Dec c, Dec d, Dec e, Dec f ) -> (a -> b -> c -> d -> e -> f -> t) -> ( String, Dec t )
opt6 ( name, a, b, c, d, e, f ) cons =
    ( name, exp6 ( name, a, b, c, d, e, f ) cons )


{-| Declares a 7 argument S-Expression state
-}
opt7 : ( String, Dec a, Dec b, Dec c, Dec d, Dec e, Dec f, Dec g ) -> (a -> b -> c -> d -> e -> f -> g -> t) -> ( String, Dec t )
opt7 ( name, a, b, c, d, e, f, g ) cons =
    ( name, exp7 ( name, a, b, c, d, e, f, g ) cons )


{-| Declares a 8 argument S-Expression state
-}
opt8 : ( String, Dec a, Dec b, Dec c, Dec d, Dec e, Dec f, Dec g, Dec h ) -> (a -> b -> c -> d -> e -> f -> g -> h -> t) -> ( String, Dec t )
opt8 ( name, a, b, c, d, e, f, g, h ) cons =
    ( name, exp8 ( name, a, b, c, d, e, f, g, h ) cons )

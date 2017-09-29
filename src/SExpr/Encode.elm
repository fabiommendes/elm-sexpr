module SExpr.Encode
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
        , tuple1
        , tuple2
        , tuple3
        , tuple4
        , tuple5
        , tuple6
        , tuple7
        , tuple8
        , tuple9
        )

{-| This module defines JSON encoders for tuples and S-expression based encoders
for arbitrary types.


# Tuple encoders

To declare a tuple encoder, simply select the appropriate tuple function and
pass the necessary decoder arguments:

    import Json.Encode exposing (..)
    import SExpr.Encode exposing (..)

    point2d =
        tuple2 float float

    encode 0 point2d (2.5, 3.1) == "[2.5, 3.1]"

@docs tuple1, tuple2, tuple3, tuple4, tuple5, tuple6, tuple7, tuple8, tuple9


# S-Expression based encoders

All S-expression encoders follow the pattern: `exp* (name, args...) linearizer`,
in which the name string represents the name for the head of the S-expression
and the linearizer function converts the desired type to a tuple.

A more concrete example:

    import Json.Encode exposing (..)
    import SExpr.Encode exposing (..)

    type alias Date =
        { year : Int
        , month : Int
        , day : Int
        }

    date =
        exp2 ( "date", int, int, int ) (\d -> ( d.year, d.month, d.day ))

    encode 0 date (Date 2017 10 1) ==  "[\"date\",2017,10,1]"

Select exp0, exp1, exp3, etc if you want a different number of arguments.

@docs exp0, exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8

-}

import Json.Encode as Enc exposing (..)


type alias Enc a =
    a -> Value



--- TUPLE ENCODERS -------------------------------------------------------------


{-| Encode an 1-tuple. This encodes a value into a list with a single element.
-}
tuple1 : Enc a -> Enc a
tuple1 enc =
    \x -> list [ enc x ]


{-| Encode an 2-tuple
-}
tuple2 : Enc a -> Enc b -> Enc ( a, b )
tuple2 enc1 enc2 =
    \( a, b ) -> list [ enc1 a, enc2 b ]


{-| Encode an 3-tuple
-}
tuple3 : Enc a -> Enc b -> Enc c -> Enc ( a, b, c )
tuple3 enc1 enc2 enc3 =
    \( a, b, c ) -> list [ enc1 a, enc2 b, enc3 c ]


{-| Encode an 4-tuple
-}
tuple4 : Enc a -> Enc b -> Enc c -> Enc d -> Enc ( a, b, c, d )
tuple4 enc1 enc2 enc3 enc4 =
    \( a, b, c, d ) -> list [ enc1 a, enc2 b, enc3 c, enc4 d ]


{-| Encode an 5-tuple
-}
tuple5 : Enc a -> Enc b -> Enc c -> Enc d -> Enc e -> Enc ( a, b, c, d, e )
tuple5 enc1 enc2 enc3 enc4 enc5 =
    \( a, b, c, d, e ) -> list [ enc1 a, enc2 b, enc3 c, enc4 d, enc5 e ]


{-| Encode an 6-tuple
-}
tuple6 : Enc a -> Enc b -> Enc c -> Enc d -> Enc e -> Enc f -> Enc ( a, b, c, d, e, f )
tuple6 enc1 enc2 enc3 enc4 enc5 enc6 =
    \( a, b, c, d, e, f ) -> list [ enc1 a, enc2 b, enc3 c, enc4 d, enc5 e, enc6 f ]


{-| Encode an 7-tuple
-}
tuple7 : Enc a -> Enc b -> Enc c -> Enc d -> Enc e -> Enc f -> Enc g -> Enc ( a, b, c, d, e, f, g )
tuple7 enc1 enc2 enc3 enc4 enc5 enc6 enc7 =
    \( a, b, c, d, e, f, g ) -> list [ enc1 a, enc2 b, enc3 c, enc4 d, enc5 e, enc6 f, enc7 g ]


{-| Encode an 8-tuple
-}
tuple8 : Enc a -> Enc b -> Enc c -> Enc d -> Enc e -> Enc f -> Enc g -> Enc h -> Enc ( a, b, c, d, e, f, g, h )
tuple8 enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 =
    \( a, b, c, d, e, f, g, h ) -> list [ enc1 a, enc2 b, enc3 c, enc4 d, enc5 e, enc6 f, enc7 g, enc8 h ]


{-| Encode an 9-tuple
-}
tuple9 : Enc a -> Enc b -> Enc c -> Enc d -> Enc e -> Enc f -> Enc g -> Enc h -> Enc i -> Enc ( a, b, c, d, e, f, g, h, i )
tuple9 enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 enc9 =
    \( a, b, c, d, e, f, g, h, i ) -> list [ enc1 a, enc2 b, enc3 c, enc4 d, enc5 e, enc6 f, enc7 g, enc8 h, enc9 i ]



--- S-EXPRESSION ENCODERS ------------------------------------------------------


{-| Encode value as a 0-argument S-expression as `["name"]`
-}
exp0 : String -> (x -> a) -> Enc x
exp0 name f =
    \x -> list [ string name ]


{-| Encode value as a 1-argument S-expression.
-}
exp1 : ( String, Enc a ) -> (x -> a) -> Enc x
exp1 ( name, enc ) decons =
    \x -> list [ string name, enc (decons x) ]


{-| Encode value as a 2-argument S-expression.
-}
exp2 : ( String, Enc a, Enc b ) -> (x -> ( a, b )) -> Enc x
exp2 ( name, enc1, enc2 ) decons =
    decons >> cons2 name >> tuple3 string enc1 enc2


{-| Encode value as a 3-argument S-expression.
-}
exp3 : ( String, Enc a, Enc b, Enc c ) -> (x -> ( a, b, c )) -> Enc x
exp3 ( name, enc1, enc2, enc3 ) decons =
    decons >> cons3 name >> tuple4 string enc1 enc2 enc3


{-| Encode value as a 4-argument S-expression.
-}
exp4 : ( String, Enc a, Enc b, Enc c, Enc d ) -> (x -> ( a, b, c, d )) -> Enc x
exp4 ( name, enc1, enc2, enc3, enc4 ) decons =
    decons >> cons4 name >> tuple5 string enc1 enc2 enc3 enc4


{-| Encode value as a 5-argument S-expression.
-}
exp5 : ( String, Enc a, Enc b, Enc c, Enc d, Enc e ) -> (x -> ( a, b, c, d, e )) -> Enc x
exp5 ( name, enc1, enc2, enc3, enc4, enc5 ) decons =
    decons >> cons5 name >> tuple6 string enc1 enc2 enc3 enc4 enc5


{-| Encode value as a 6-argument S-expression.
-}
exp6 : ( String, Enc a, Enc b, Enc c, Enc d, Enc e, Enc f ) -> (x -> ( a, b, c, d, e, f )) -> Enc x
exp6 ( name, enc1, enc2, enc3, enc4, enc5, enc6 ) decons =
    decons >> cons6 name >> tuple7 string enc1 enc2 enc3 enc4 enc5 enc6


{-| Encode value as a 7-argument S-expression.
-}
exp7 : ( String, Enc a, Enc b, Enc c, Enc d, Enc e, Enc f, Enc g ) -> (x -> ( a, b, c, d, e, f, g )) -> Enc x
exp7 ( name, enc1, enc2, enc3, enc4, enc5, enc6, enc7 ) decons =
    decons >> cons7 name >> tuple8 string enc1 enc2 enc3 enc4 enc5 enc6 enc7


{-| Encode value as a 8-argument S-expression.
-}
exp8 : ( String, Enc a, Enc b, Enc c, Enc d, Enc e, Enc f, Enc g, Enc h ) -> (x -> ( a, b, c, d, e, f, g, h )) -> Enc x
exp8 ( name, enc1, enc2, enc3, enc4, enc5, enc6, enc7, enc8 ) decons =
    decons >> cons8 name >> tuple9 string enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8



--- UTILITIES ------------------------------------------------------------------
-- Joining the head of a tuple


cons2 : a -> ( b, c ) -> ( a, b, c )
cons2 a ( b, c ) =
    ( a, b, c )


cons3 : a -> ( b, c, d ) -> ( a, b, c, d )
cons3 a ( b, c, d ) =
    ( a, b, c, d )


cons4 : a -> ( b, c, d, e ) -> ( a, b, c, d, e )
cons4 a ( b, c, d, e ) =
    ( a, b, c, d, e )


cons5 : a -> ( b, c, d, e, f ) -> ( a, b, c, d, e, f )
cons5 a ( b, c, d, e, f ) =
    ( a, b, c, d, e, f )


cons6 : a -> ( b, c, d, e, f, g ) -> ( a, b, c, d, e, f, g )
cons6 a ( b, c, d, e, f, g ) =
    ( a, b, c, d, e, f, g )


cons7 : a -> ( b, c, d, e, f, g, h ) -> ( a, b, c, d, e, f, g, h )
cons7 a ( b, c, d, e, f, g, h ) =
    ( a, b, c, d, e, f, g, h )


cons8 : a -> ( b, c, d, e, f, g, h, i ) -> ( a, b, c, d, e, f, g, h, i )
cons8 a ( b, c, d, e, f, g, h, i ) =
    ( a, b, c, d, e, f, g, h, i )

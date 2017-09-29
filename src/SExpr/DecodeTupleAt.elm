module SExpr.DecodeTupleAt exposing (..)

import Array
import Json.Decode exposing (..)


type alias Dec a =
    Decoder a


tuple1At : Int -> Dec a -> Dec a
tuple1At i dec =
    index i dec


tuple2At : Int -> Dec a -> Dec b -> Dec ( a, b )
tuple2At i dec1 dec2 =
    map2 (\a b -> ( a, b ))
        (index i dec1)
        (index (i + 1) dec2)


tuple3At : Int -> Dec a -> Dec b -> Dec c -> Dec ( a, b, c )
tuple3At i dec1 dec2 dec3 =
    map3 (\a b c -> ( a, b, c ))
        (index i dec1)
        (index (i + 1) dec2)
        (index (i + 2) dec3)


tuple4At : Int -> Dec a -> Dec b -> Dec c -> Dec d -> Dec ( a, b, c, d )
tuple4At i dec1 dec2 dec3 dec4 =
    map4 (\a b c d -> ( a, b, c, d ))
        (index i dec1)
        (index (i + 1) dec2)
        (index (i + 2) dec3)
        (index (i + 3) dec4)


tuple5At : Int -> Dec a -> Dec b -> Dec c -> Dec d -> Dec e -> Dec ( a, b, c, d, e )
tuple5At i dec1 dec2 dec3 dec4 dec5 =
    map5 (\a b c d e -> ( a, b, c, d, e ))
        (index i dec1)
        (index (i + 1) dec2)
        (index (i + 2) dec3)
        (index (i + 3) dec4)
        (index (i + 4) dec5)


tuple6At : Int -> Dec a -> Dec b -> Dec c -> Dec d -> Dec e -> Dec f -> Dec ( a, b, c, d, e, f )
tuple6At i dec1 dec2 dec3 dec4 dec5 dec6 =
    map6 (\a b c d e f -> ( a, b, c, d, e, f ))
        (index i dec1)
        (index (i + 1) dec2)
        (index (i + 2) dec3)
        (index (i + 3) dec4)
        (index (i + 4) dec5)
        (index (i + 5) dec6)


tuple7At : Int -> Dec a -> Dec b -> Dec c -> Dec d -> Dec e -> Dec f -> Dec g -> Dec ( a, b, c, d, e, f, g )
tuple7At i dec1 dec2 dec3 dec4 dec5 dec6 dec7 =
    map7 (\a b c d e f g -> ( a, b, c, d, e, f, g ))
        (index i dec1)
        (index (i + 1) dec2)
        (index (i + 2) dec3)
        (index (i + 3) dec4)
        (index (i + 4) dec5)
        (index (i + 5) dec6)
        (index (i + 6) dec7)


tuple8At : Int -> Dec a -> Dec b -> Dec c -> Dec d -> Dec e -> Dec f -> Dec g -> Dec h -> Dec ( a, b, c, d, e, f, g, h )
tuple8At i dec1 dec2 dec3 dec4 dec5 dec6 dec7 dec8 =
    map8 (\a b c d e f g h -> ( a, b, c, d, e, f, g, h ))
        (index i dec1)
        (index (i + 1) dec2)
        (index (i + 2) dec3)
        (index (i + 3) dec4)
        (index (i + 4) dec5)
        (index (i + 5) dec6)
        (index (i + 6) dec7)
        (index (i + 7) dec8)


tuple9At : Int -> Dec a -> Dec b -> Dec c -> Dec d -> Dec e -> Dec f -> Dec g -> Dec h -> Dec i -> Dec ( a, b, c, d, e, f, g, h, i )
tuple9At i dec1 dec2 dec3 dec4 dec5 dec6 dec7 dec8 dec9 =
    let
        head =
            map4 (\a b c d -> ( a, b, c, d ))
                (index i dec1)
                (index (i + 1) dec2)
                (index (i + 2) dec3)
                (index (i + 3) dec4)

        tail =
            map5 (\a b c d e -> ( a, b, c, d, e ))
                (index (i + 4) dec5)
                (index (i + 5) dec6)
                (index (i + 6) dec7)
                (index (i + 7) dec8)
                (index (i + 8) dec9)
    in
    map2 (\( a, b, c, d ) ( e, f, g, h, ii ) -> ( a, b, c, d, e, f, g, h, ii ))
        head
        tail



--- UTILITY FUNCTIONS ----------------------------------------------------------


check : String -> String -> (() -> a) -> Decoder a
check head given f =
    if head == given then
        succeed <| f ()
    else
        fail ("invalid head: " ++ given)


withSize : Int -> Dec a -> Dec a
withSize n dec =
    map2 (\x y -> x)
        dec
        (array value
            |> andThen
                (\lst ->
                    if Array.length lst == n then
                        succeed True
                    else
                        fail <|
                            "expect an "
                                ++ toString n
                                ++ "-length array, got "
                                ++ toString (Array.length lst)
                )
        )

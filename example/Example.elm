module Main exposing (..)

import Html exposing (Html, code, div, h1, h2, p, pre, text)
import Json.Decode exposing (..)
import Json.Encode as Enc
import SExpr.Decode as D exposing (..)
import SExpr.Encode as E exposing (..)


-- The base data type


type alias Date =
    { year : Int, month : Int, day : Int }



-- Declare decoder


dateDecoder : Decoder Date
dateDecoder =
    D.exp3 ( "date", int, int, int ) Date


dateEncoder : Date -> Value
dateEncoder =
    E.exp3 ( "date", Enc.int, Enc.int, Enc.int )
        (\d -> ( d.year, d.month, d.day ))



-- Declares a JSON


examples : List String
examples =
    [ "[\"date\", 2017, 10, 1]"
    , "[\"date\", 2017, 10, 1, 10]"
    , "[\"date\", 2017, 10]"
    ]



-- Render form with Input helpers


view : List String -> Html Bool
view examples =
    let
        strings =
            List.map (decodeString dateDecoder >> toString) examples
    in
    div []
        -- Encoding text
        [ h1 [] [ text "Decoding" ]
        , div [] (flip List.map strings (\st -> pre [] [ text st ]))

        -- Decoding text
        , h1 [] [ text "Encoding" ]
        , pre [] [ text <| Enc.encode 0 <| dateEncoder (Date 2017 10 1) ]
        ]


main =
    Html.program
        { init = ( examples, Cmd.none )
        , update = \msg f -> ( f, Cmd.none )
        , view = view
        , subscriptions = \_ -> Sub.none
        }

module Main exposing (main)

{-|

@docs Parser, try

@docs Token, token, keyword, symbol, OnError, skip, fastForwardTo, stopWith

@docs succeed, ignore, keep

@docs oneOf

@docs chompWhile

@docs map

-}

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick, onInput)
import Parser.Advanced as PA
import Parser.Recoverable as PR exposing (Outcome(..), Parser)


type alias Model =
    { input : String
    , parsed : Outcome Never Problem AST
    }


initialModel : Model
initialModel =
    { input = ""
    , parsed = Failure []
    }


type Msg
    = NewInput String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewInput val ->
            { model
                | input = val
                , parsed = PR.run parser val
            }


view : Model -> Html Msg
view model =
    div []
        [ Html.input [ onInput NewInput ] [ text <| model.input ]
        , Html.br [] []
        , Debug.toString model.parsed |> text
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }



-- Example Parser


type AST
    = ParsedOk (List Int)


type Problem
    = ExpectingStartBrace
    | ExpectingEndBrace
    | ExpectingComma
    | ExpectingInt
    | InvalidNumber
    | Recovered String


parser : Parser Never Problem AST
parser =
    PR.succeed ParsedOk
        |> PR.keep
            (PR.sequence
                { start = ( "(", ExpectingStartBrace )
                , end = ( ")", ExpectingEndBrace )
                , separator = ( ",", ExpectingComma )
                , spaces = PR.spaces
                , item = PR.int ExpectingInt InvalidNumber
                , trailing = PR.Mandatory
                }
            )



-- |> PR.forwardOrSkip [ ')' ] Recovered

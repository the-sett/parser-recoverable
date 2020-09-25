module ArrayOfInts exposing (main)

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
import Maybe.Extra
import Parser.Advanced as PA exposing ((|.), (|=))
import Parser.Recoverable as PR exposing (Outcome(..))


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
        [ Html.text "Try entering an array of integers, like [ 1, 2, 3 ]."
        , Html.br [] []
        , Html.input [ onInput NewInput ] [ text <| model.input ]
        , Html.br [] []
        , Html.pre [] [ Debug.toString model.parsed |> text ]
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
    = ExpectingEnd
    | ExpectingSpace
    | ExpectingComma
    | ExpectingLSqBracket
    | ExpectingRSqBracket
    | ExpectingInt
    | InvalidNumber
    | Discarded String String


parser : PR.Parser Never Problem AST
parser =
    PR.succeed ParsedOk
        |> PR.keep sequence


sequence : PR.Parser Never Problem (List Int)
sequence =
    PR.sequence
        { start = ( "[", ExpectingLSqBracket )
        , separator = ( ",", ExpectingComma )
        , end = ( "]", ExpectingRSqBracket )
        , spaces = PR.spaces
        , item = PR.int ExpectingInt InvalidNumber
        , trailing = PR.Forbidden
        }

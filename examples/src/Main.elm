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
    | ExpectingEnd
    | ExpectingSpace
    | ExpectingInt
    | InvalidNumber
    | Discarded String


parser : PR.Parser Never Problem AST
parser =
    PR.succeed ParsedOk
        |> PR.keep loop


loop : PR.Parser Never Problem (List Int)
loop =
    PR.loop []
        (\vals ->
            PR.oneOf
                [ PR.succeed ()
                    |> PR.ignore (PR.end ExpectingEnd)
                    |> PR.map (\_ -> PR.Done (List.reverse vals))
                , PR.succeed (\val -> val :: vals |> PR.Loop)
                    |> PR.keep
                        (PR.succeed identity
                            |> PR.ignore PR.spaces
                            |> PR.keep (PR.int ExpectingInt InvalidNumber |> PR.map Just)
                            |> PR.ignore PR.spaces
                            |> PR.forwardOrSkip Nothing [ ' ' ] ExpectingSpace Discarded
                        )
                ]
        )
        |> PR.map Maybe.Extra.values

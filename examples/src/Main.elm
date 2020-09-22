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



-- parser : Parser Never Problem AST
-- parser =
--     PR.succeed ParsedOk
--         |> PR.keep
--             (PR.sequence
--                 { start = ( "(", ExpectingStartBrace )
--                 , end = ( ")", ExpectingEndBrace )
--                 , separator = ( ",", ExpectingComma )
--                 , spaces = PR.spaces
--                 , item = PR.int ExpectingInt InvalidNumber
--                 , trailing = PR.Mandatory
--                 }
--             )


parser : Parser Never Problem AST
parser =
    PR.succeed ParsedOk
        |> PR.keep
            (PR.loop []
                (\vals ->
                    PR.succeed
                        (\val ->
                            if List.length vals < 2 then
                                val :: vals |> PR.Loop

                            else
                                val :: vals |> PR.Done
                        )
                        |> PR.ignore PR.spaces
                        |> PR.keep (PR.int ExpectingInt InvalidNumber)
                        |> PR.ignore PR.spaces
                        |> PR.ignore (PR.symbol "," ExpectingComma)
                        |> PR.forwardOrSkip [ ',' ] Recovered
                )
            )



-- |> PR.forwardOrSkip [ ')' ] Recovered

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
import Parser.Advanced as PA exposing ((|.), (|=))
import Parser.Recoverable as PR exposing (Outcome(..))


type alias Model =
    { input : String
    , parsed : Result (List (PA.DeadEnd Never Problem)) AST
    }


initialModel : Model
initialModel =
    { input = ""
    , parsed = Err []
    }


type Msg
    = NewInput String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewInput val ->
            { model
                | input = val
                , parsed = PA.run parser val
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
--
--
--
-- parser : PR.Parser Never Problem AST
-- parser =
--     PR.succeed ParsedOk
--         |> PR.keep
--             (PR.loop []
--                 (\vals ->
--                     PR.succeed
--                         (\val ->
--                             if List.length vals < 2 then
--                                 val :: vals |> PR.Loop
--
--                             else
--                                 val :: vals |> PR.Done
--                         )
--                         |> PR.ignore PR.spaces
--                         |> PR.keep (PR.int ExpectingInt InvalidNumber)
--                         |> PR.ignore PR.spaces
--                         |> PR.forwardThenRetry [ ',' ] Recovered
--                         |> PR.ignore (PR.symbol "," ExpectingComma)
--                 )
--             )


parser : PA.Parser Never Problem AST
parser =
    PA.succeed ParsedOk
        |= PA.loop []
            (\vals ->
                PA.succeed
                    (\val ->
                        if List.length vals < 2 then
                            val :: vals |> PA.Loop

                        else
                            val :: vals |> PA.Done
                    )
                    |= ((PA.succeed identity
                            |. PA.spaces
                            |= PA.int ExpectingInt InvalidNumber
                            |. PA.spaces
                            |. PA.symbol (PA.Token "," ExpectingComma)
                        )
                            |> forwardThenRetry [ ',' ] Recovered
                       )
            )


{-| Nailed it
-}
forwardThenRetry : List Char -> (String -> x) -> PA.Parser c x a -> PA.Parser c x a
forwardThenRetry matches probFn parsr =
    PA.loop ()
        (\_ ->
            PA.oneOf
                [ PA.succeed (\val -> val |> PA.Done)
                    |= PA.backtrackable parsr
                , chompTill matches probFn
                    |> PA.andThen
                        (\( foundMatch, chompedString, ( row, col ) ) ->
                            if chompedString == "" then
                                PA.problem (probFn "")
                                --    |> PA.Done
                                --

                            else
                                PA.Loop ()
                                    |> PA.succeed
                        )
                ]
        )


chompTill : List Char -> (String -> x) -> PA.Parser c x ( Bool, String, ( Int, Int ) )
chompTill chars prob =
    PA.succeed (\pos val flag -> ( flag, val, pos ))
        |= PA.getPosition
        |= (PA.chompWhile (\c -> not <| List.member (Debug.log "skipping" c) chars)
                |> PA.getChompedString
           )
        |= PA.oneOf
            [ PA.map (always True)
                (PA.chompIf (\c -> List.member (Debug.log "matched" c) chars) (prob ""))
            , PA.succeed False
            ]

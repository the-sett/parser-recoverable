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
    = ParsedSuccesfully


type Problem
    = Problem
    | StartBrace
    | EndBrace
    | Recovered String


parser : Parser Never Problem AST
parser =
    PR.succeed ParsedSuccesfully
        |> PR.ignore (PA.Token "(" StartBrace |> PR.symbol)
        |> PR.ignore (PA.Token ")" EndBrace |> PR.symbol |> PR.withRecovery (PR.ChompForMatchOrSkip [ ')' ] Recovered))



--|> PR.ignore (PA.Token ")" EndBrace |> PR.symbol |> PR.withRecovery (PR.ChompForMatch [ ')' ] EndBrace))
--|> PR.ignore (PA.Token ")" EndBrace |> PR.symbol |> PR.withRecovery PR.Ignore)
--|> PR.ignore (PA.Token ")" EndBrace |> PR.symbol |> PR.withRecovery (PR.Warn EndBrace))
--|> PR.ignore (PA.Token ")" EndBrace |> PR.symbol |> PR.withRecovery PR.Fail)

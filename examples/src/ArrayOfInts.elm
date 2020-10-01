module ArrayOfInts exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (checked, name, type_)
import Html.Events exposing (onClick, onInput)
import Maybe.Extra
import Parser.Advanced as PA exposing ((|.), (|=))
import Parser.Recoverable as PR
    exposing
        ( Outcome(..)
        , Parser
        , Step(..)
        , Trailing(..)
        , andThen
        , ignore
        , keep
        , loop
        , map
        , oneOf
        , succeed
        , token
        )
import Parser.Recoverable.Sequence as PRS


type alias Model =
    { input : String
    , parsed : Outcome Never Problem AST
    , trailing : PR.Trailing
    }


initialModel : Model
initialModel =
    { input = ""
    , parsed = Failure []
    , trailing = PR.Forbidden
    }


type Msg
    = NewInput String
    | SwitchTo PR.Trailing String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewInput val ->
            { model
                | input = val
                , parsed = PR.run (parser model.trailing) val
            }

        SwitchTo trailing _ ->
            { model
                | trailing = trailing
                , parsed = PR.run (parser trailing) model.input
            }


view : Model -> Html Msg
view model =
    div []
        [ Html.text "Try entering an array of integers, like [ 1, 2, 3 ]."
        , Html.br [] []
        , div []
            [ radio "Forbidden" (model.trailing == Forbidden) (SwitchTo Forbidden)
            , radio "Optional" (model.trailing == Optional) (SwitchTo Optional)
            , radio "Mandatory" (model.trailing == Mandatory) (SwitchTo Mandatory)
            ]
        , Html.br [] []
        , Html.input [ onInput NewInput ] [ text <| model.input ]
        , Html.br [] []
        , Html.pre [] [ Debug.toString model.parsed |> text ]
        ]


radio : String -> Bool -> (String -> msg) -> Html msg
radio value isChecked msg =
    Html.label []
        [ Html.input [ type_ "radio", onInput msg, checked isChecked ] []
        , text value
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


parser : PR.Trailing -> PR.Parser Never Problem AST
parser trailing =
    PR.succeed ParsedOk
        |> PR.keep (sequence trailing)


sequence : PR.Trailing -> PR.Parser Never Problem (List Int)
sequence trailing =
    PRS.sequence
        { start = "["
        , startProb = ExpectingLSqBracket
        , separator = ","
        , separatorProb = ExpectingComma
        , end = "]"
        , endProb = ExpectingRSqBracket
        , spaces = PR.spaces
        , forwardProb = Discarded
        , item = PR.int ExpectingInt InvalidNumber
        , trailing = trailing
        }

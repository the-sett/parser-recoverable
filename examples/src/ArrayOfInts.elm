module ArrayOfInts exposing (main)

import Browser
import Html exposing (Html, button, div, text)
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
    sequenceRec
        { start = "["
        , startProb = ExpectingLSqBracket
        , separator = ","
        , separatorProb = ExpectingComma
        , end = "]"
        , endProb = ExpectingRSqBracket
        , spaces = PR.spaces
        , forwardProb = Discarded
        , item = PR.int ExpectingInt InvalidNumber
        , trailing = PR.Forbidden
        }



--- === === === ===


sequenceRec :
    { start : String
    , startProb : x
    , separator : String
    , separatorProb : x
    , end : String
    , endProb : x
    , spaces : Parser c x ()
    , forwardProb : String -> String -> x
    , item : Parser c x a
    , trailing : Trailing
    }
    -> Parser c x (List a)
sequenceRec seqDef =
    succeed identity
        |> ignore (token seqDef.start seqDef.startProb)
        |> ignore seqDef.spaces
        |> keep
            (sequenceEnd
                (token seqDef.end seqDef.endProb)
                seqDef.spaces
                seqDef.item
                (token seqDef.separator seqDef.separatorProb)
                seqDef.trailing
                seqDef.separatorProb
                seqDef.forwardProb
            )


sequenceEnd :
    Parser c x ()
    -> Parser c x ()
    -> Parser c x a
    -> Parser c x ()
    -> Trailing
    -> x
    -> (String -> String -> x)
    -> Parser c x (List a)
sequenceEnd ender ws parseItem sep trailing noMatchProb forwardProb =
    let
        chompRest item =
            case trailing of
                Forbidden ->
                    loop [ Just item ] (sequenceEndForbidden ender ws parseItem sep noMatchProb forwardProb)
                        |> PR.map Maybe.Extra.values

                Optional ->
                    loop [ item ] (sequenceEndOptional ender ws parseItem sep)

                Mandatory ->
                    succeed identity
                        |> ignore ws
                        |> ignore sep
                        |> ignore ws
                        |> keep (loop [ item ] (sequenceEndMandatory ws parseItem sep))
                        |> ignore ender
    in
    oneOf
        [ parseItem
            |> andThen chompRest
        , ender
            |> map (\_ -> [])
        ]


sequenceEndForbidden :
    Parser c x ()
    -> Parser c x ()
    -> Parser c x a
    -> Parser c x ()
    -> x
    -> (String -> String -> x)
    -> List (Maybe a)
    -> Parser c x (Step (List (Maybe a)) (List (Maybe a)))
sequenceEndForbidden ender ws parseItem sep noMatchProb forwardProb revItems =
    succeed identity
        |> ignore ws
        |> keep
            (oneOf
                [ ender
                    |> map (\_ -> Done (List.reverse revItems))
                , succeed (\item -> item :: revItems |> PR.Loop)
                    |> PR.keep
                        (succeed Just
                            |> ignore sep
                            |> ignore ws
                            |> keep parseItem
                            |> PR.forward Nothing [ "," ] noMatchProb forwardProb
                        )
                ]
            )


sequenceEndOptional :
    Parser c x ()
    -> Parser c x ()
    -> Parser c x a
    -> Parser c x ()
    -> List a
    -> Parser c x (Step (List a) (List a))
sequenceEndOptional ender ws parseItem sep revItems =
    let
        parseEnd =
            map (\_ -> Done (List.reverse revItems)) ender
    in
    succeed identity
        |> ignore ws
        |> keep
            (oneOf
                [ succeed identity
                    |> ignore sep
                    |> ignore ws
                    |> keep
                        (oneOf
                            [ parseItem |> map (\item -> Loop (item :: revItems))
                            , parseEnd
                            ]
                        )
                , parseEnd
                ]
            )


sequenceEndMandatory :
    Parser c x ()
    -> Parser c x a
    -> Parser c x ()
    -> List a
    -> Parser c x (Step (List a) (List a))
sequenceEndMandatory ws parseItem sep revItems =
    oneOf
        [ succeed identity
            |> keep parseItem
            |> ignore ws
            |> ignore sep
            |> ignore ws
            |> map (\item -> Loop (item :: revItems))
        , succeed ()
            |> map (\_ -> Done (List.reverse revItems) |> Debug.log "mandatory end")
        ]

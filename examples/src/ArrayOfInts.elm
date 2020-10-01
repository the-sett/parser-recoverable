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
    sequenceLoop
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
--
-- Sequence is: Start ( WS Val WS Sep )* End
--
-- Error during this - Fast forward to Sep or End
--
-- Sequence like []
-- FOM -> Success
--
-- Sequence like [...
-- FOM -> Partial (Skipped End)
--
-- Sequence like [1] OR [1,..,N]
-- FO -> Success
-- M -> Partial (Skipped Separator)
--
-- Sequence like [X] OR [1,..,N,X]
-- FO -> Partial (list, skipped X)
-- M -> Partial (list, skipped X)
--  AND Partial (Skipped Separator)
--
-- Sequence like [1,] OR [1,..,N,]
-- F -> Partial (Fast Forward Separator)
-- MO -> Success
--
-- Sequence like [X,] OR [1,..,N,X,]
-- F -> Partial (list, skipped X)
--  AND Partial (FastForward Separator)
-- MO -> Partial (list, skipped X)
--


sequenceLoop :
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
sequenceLoop seq =
    let
        startParser =
            PR.symbol seq.start seq.startProb

        endParser =
            PR.oneOf
                [ PR.symbol seq.end seq.endProb
                , PR.end seq.endProb |> PR.andThen (\() -> partial () seq.endProb)
                ]

        sepOrEndParser =
            case seq.trailing of
                Forbidden ->
                    PR.oneOf
                        [ PR.succeed identity
                            |> PR.keep PR.getPosition
                            |> PR.ignore (PR.symbol seq.separator seq.separatorProb)
                            |> PR.ignore seq.spaces
                            |> PR.ignore (PR.symbol seq.end seq.endProb)
                            |> PR.backtrackable
                            |> PR.andThen (\pos -> partialAt pos False seq.endProb)
                        , PR.succeed False
                            |> PR.ignore seq.spaces
                            |> PR.ignore (PR.symbol seq.end seq.endProb)
                            |> PR.backtrackable
                        , PR.succeed True
                            |> PR.ignore (PR.symbol seq.separator seq.separatorProb)
                        ]

                Optional ->
                    PR.oneOf
                        [ PR.succeed False
                            |> PR.ignore (PR.symbol seq.separator seq.separatorProb |> PR.optional ())
                            |> PR.ignore seq.spaces
                            |> PR.ignore (PR.symbol seq.end seq.endProb)
                            |> PR.backtrackable
                        , PR.succeed True
                            |> PR.ignore (PR.symbol seq.separator seq.separatorProb)
                        ]

                Mandatory ->
                    PR.oneOf
                        [ PR.succeed False
                            |> PR.ignore (PR.symbol seq.separator seq.separatorProb |> PR.skip () seq.separatorProb)
                            |> PR.ignore seq.spaces
                            |> PR.ignore (PR.symbol seq.end seq.endProb)
                            |> PR.backtrackable
                        , PR.succeed True
                            |> PR.ignore (PR.symbol seq.separator seq.separatorProb)
                        ]

        loopParser =
            PR.loop []
                (\vals ->
                    PR.oneOf
                        [ PR.succeed ()
                            |> PR.ignore endParser
                            |> PR.map (\_ -> PR.Done (List.reverse vals))
                        , PR.succeed
                            (\( ( val, cont1 ), cont2 ) ->
                                let
                                    _ =
                                        Debug.log "cont1" cont1

                                    _ =
                                        Debug.log "cont2" cont2
                                in
                                case cont1 && cont2 of
                                    True ->
                                        val :: vals |> PR.Loop

                                    False ->
                                        val :: vals |> List.reverse |> PR.Done
                            )
                            |> PR.keep
                                (PR.succeed (\maybeItem cont -> ( maybeItem, cont ))
                                    |> PR.ignore seq.spaces
                                    |> PR.keep (seq.item |> PR.map Just)
                                    |> PR.ignore seq.spaces
                                    |> PR.keep sepOrEndParser
                                    |> forwardToSepOrEnd ( Nothing, True ) [ seq.separator ] [ seq.end ] seq.separatorProb seq.forwardProb
                                )
                        ]
                )
                |> PR.map Maybe.Extra.values
    in
    PR.succeed identity
        |> PR.ignore startParser
        |> PR.keep loopParser


forwardToSepOrEnd :
    a
    -> List String
    -> List String
    -> x
    -> (String -> String -> x)
    -> Parser c x a
    -> Parser c x ( a, Bool )
forwardToSepOrEnd val matches endMatches noMatchProb chompedProb prser =
    PA.oneOf
        [ PA.backtrackable prser |> PR.map (\res -> ( res, True ))
        , chompTillSepOrEnd matches endMatches noMatchProb noMatchProb
            |> PA.andThen
                (\res ->
                    case res.matched of
                        FFCont ->
                            partialAt ( res.row, res.col )
                                ( val, True )
                                (chompedProb res.discarded res.sentinal)

                        FFEnd ->
                            partialAt ( res.row, res.col )
                                ( val, False )
                                (chompedProb res.discarded res.sentinal)

                        FFNone ->
                            PA.problem noMatchProb
                )
        ]
        |> PA.map (Debug.log "forward")


type FFMatch
    = FFCont
    | FFEnd
    | FFNone


type alias FastForward =
    { matched : FFMatch
    , discarded : String
    , sentinal : String
    , row : Int
    , col : Int
    }


chompTillSepOrEnd :
    List String
    -> List String
    -> x
    -> x
    -> PA.Parser c x FastForward
chompTillSepOrEnd tokens endTokens prob endProb =
    let
        chars =
            List.map (\val -> String.uncons val |> Maybe.map Tuple.first) (tokens ++ endTokens)
                |> values

        tokenParsers =
            List.map (\val -> PA.token (PA.Token val prob) |> PA.getChompedString) tokens

        endTokenParsers =
            List.map (\val -> PA.token (PA.Token val prob) |> PA.getChompedString) endTokens
    in
    case chars of
        [] ->
            PA.problem prob

        _ ->
            PA.loop ""
                (\discardAccum ->
                    PA.succeed
                        (\( row, col ) discardStep ( matched, sentinal ) ->
                            let
                                discarded =
                                    discardAccum ++ discardStep
                            in
                            case matched of
                                FFCont ->
                                    PA.Done
                                        { matched = matched
                                        , discarded = discarded
                                        , sentinal = sentinal
                                        , row = row
                                        , col = col
                                        }
                                        |> Debug.log "chompTillSepOrEnd - FFCont matched"

                                FFEnd ->
                                    PA.Done
                                        { matched = matched
                                        , discarded = discarded
                                        , sentinal = sentinal
                                        , row = row
                                        , col = col
                                        }
                                        |> Debug.log "chompTillSepOrEnd - FFEnd matched"

                                FFNone ->
                                    if discardStep == "" then
                                        PA.Done
                                            { matched = matched
                                            , discarded = discarded
                                            , sentinal = sentinal
                                            , row = row
                                            , col = col
                                            }
                                            |> Debug.log "chompTillSepOrEnd - FFNone discarded \"\""

                                    else
                                        PA.Loop discarded
                                            |> Debug.log "chompTillSepOrEnd - FFNone looping"
                        )
                        |= PA.getPosition
                        |= (PA.chompWhile (\c -> not <| List.member c chars)
                                |> PA.getChompedString
                           )
                        |= PA.oneOf
                            [ PA.succeed (\chompedString -> ( FFEnd, chompedString ))
                                |= PA.oneOf endTokenParsers
                            , PA.succeed (\chompedString -> ( FFCont, chompedString ))
                                |= PA.oneOf tokenParsers
                            , PA.succeed ( FFNone, "" )
                            ]
                )


partialAt : ( Int, Int ) -> a -> x -> PA.Parser c x (Outcome c x a)
partialAt ( row, col ) val prob =
    Partial
        [ { row = row
          , col = col
          , problem = prob
          , contextStack = []
          }
        ]
        val
        |> PA.succeed


partial : a -> x -> PA.Parser c x (Outcome c x a)
partial val prob =
    PA.succeed
        (\( row, col ) ->
            Partial
                [ { row = row
                  , col = col
                  , problem = prob
                  , contextStack = []
                  }
                ]
                val
        )
        |= PA.getPosition
        |> PA.map (Debug.log "partial")


values : List (Maybe a) -> List a
values maybeList =
    List.foldr
        (\maybeVal accum ->
            case maybeVal of
                Just val ->
                    val :: accum

                Nothing ->
                    accum
        )
        []
        maybeList

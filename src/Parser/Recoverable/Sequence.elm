module Parser.Recoverable.Sequence exposing (sequence)

{-| Recoverable sequences are a variation on `Parser.Advanved.sequence` that
allow errors in the sequence items and recover by fast forwarding to a separator
or end token.

@docs sequence

-}

import Parser.Advanced as PA exposing ((|.), (|=))
import Parser.Common exposing (partial, partialAt, values)
import Parser.Recoverable as PR exposing (Parser, Trailing(..))
import Parser.Recoverable.Tactics as PRT



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
-- Sequence like [...X
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


{-| Like `Parser.Advanced.sequence` but it can skip over items by fast
forwarding to the next separator or end token.

When something is fast fowarded over, the `forwardProb` function is used to
built the error that is added to the `Partial` result. This will be given the
string skipped over, and the token consumed to get back to the restart point.

-}
sequence :
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
sequence seq =
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
                            |> PR.ignore endParser
                            |> PR.backtrackable
                            |> PR.andThen (\pos -> partialAt pos False seq.endProb)
                        , PR.succeed False
                            |> PR.ignore seq.spaces
                            |> PR.ignore endParser
                            |> PR.backtrackable
                        , PR.succeed True
                            |> PR.ignore (PR.symbol seq.separator seq.separatorProb)
                        ]

                Optional ->
                    PR.oneOf
                        [ PR.succeed False
                            |> PR.ignore (PR.symbol seq.separator seq.separatorProb |> PRT.optional ())
                            |> PR.ignore seq.spaces
                            |> PR.ignore endParser
                            |> PR.backtrackable
                        , PR.succeed True
                            |> PR.ignore (PR.symbol seq.separator seq.separatorProb)
                        ]

                Mandatory ->
                    PR.oneOf
                        [ PR.succeed False
                            |> PR.ignore (PR.symbol seq.separator seq.separatorProb |> PRT.skip () seq.separatorProb)
                            |> PR.ignore seq.spaces
                            |> PR.ignore endParser
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
                |> PR.map values
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
                            partialAt ( res.row, res.col )
                                ( val, False )
                                (chompedProb res.discarded res.sentinal)
                )
        ]


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

                                FFEnd ->
                                    PA.Done
                                        { matched = matched
                                        , discarded = discarded
                                        , sentinal = sentinal
                                        , row = row
                                        , col = col
                                        }

                                FFNone ->
                                    if discardStep == "" then
                                        PA.Done
                                            { matched = matched
                                            , discarded = discarded
                                            , sentinal = sentinal
                                            , row = row
                                            , col = col
                                            }

                                    else
                                        PA.Loop discarded
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

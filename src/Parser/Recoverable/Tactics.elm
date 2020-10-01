module Parser.Recoverable.Tactics exposing (optional, skip, forward, forwardOrSkip)

{-| Parser recovery tactics that need to be manually added to parsing code.


# Recovery Tactics with Default Value

For these recovery tactics, a default value for `a` must be given, so that the
parser can return something in the event of an error.

The aim is not to error correct parsed values (like an integer), but to get the
parser back to a state where it can continue running, whilst skipping out some
part of the input. For this reason, a value type for something being parsed out
of the text, such as an `Int` or `String`, is not such a useful choice here.

If skipping over some keyword or symbol or chomping characters, the parser will be
of the form `Parser c x ()`. In this case, it is easy to give `()` as the default
value. The symbol or keyword will be counted as being there silently, with a warning,
or will act as a sentinal token that parsing will forward to, then try to continue.

Another common way to recover is when parsing a sequence of things, to skip any
things which are not syntactically correct. In this situation you might use
`Nothing` as the default value. You can use `Maybe.Extra.value` to convert a
`List (Maybe a)` to a `List a`, in this case.

Some other useful default values might be `[]`, or `Dict.empty` or `Set.empty`
and so on.

@docs optional, skip, forward, forwardOrSkip

-}

import Parser.Advanced as PA exposing ((|.), (|=))
import Parser.Common exposing (addWarnings, failure, partial, partialAt, values)
import Parser.Recoverable as PR exposing (Outcome(..), Parser, Trailing(..), oneOf)


{-| Silently ignore any failure.

A default value for `a` must be given, so that the parser can return something
in the event of an error and succesful recovery.

-}



-- Error Recovery Tactics


optional : a -> Parser c x a -> Parser c x a
optional val parser =
    PA.oneOf
        [ parser
        , PA.succeed (Success val)
        ]


{-| Skip over a failure, but when one happens add an error to a `Partial` result.

A default value for `a` must be given, so that the parser can return something
in the event of an error and succesful recovery.

-}
skip : a -> x -> Parser c x a -> Parser c x a
skip val prob parser =
    oneOf
        [ parser
        , partial val prob
        ]


{-| When parsing fails, attempt to fast-forward to one of a set of sentinal tokens.
When this happens an error is added to a `Partial` result.

When called with these arguments:

    forward val matches noMatchProb chompedProb parser

Note that the `chompedProb` argument has the type `(String -> String -> x)`.
This is called with the string being skipped over, and the matched token being
consumed. This information will be combined with its position in the input text,
in the error added to the `Partial` result.

A default value for `a` must be given, so that the parser can return something
in the event of an error and succesful recovery.

-}
forward : a -> List String -> x -> (String -> String -> x) -> Parser c x a -> Parser c x a
forward val matches noMatchProb chompedProb parser =
    PA.oneOf
        [ PA.backtrackable parser
        , chompTillToken matches noMatchProb
            |> PA.andThen
                (\res ->
                    if res.matched then
                        partialAt ( res.row, res.col )
                            val
                            (chompedProb res.discarded res.sentinal)

                    else
                        failure noMatchProb
                )
        ]


{-| When parsing fails, attempt to fast-forward to one of a set of sentinal tokens,
or if none can be found, skip over the failure. When this happens an error is
added to a `Partial` result.

Note that the `chompedProb` argument has the type `(String -> String -> x)`.
This is called with the string being skipped over, and the matched token being
consumed. If no match is found, then `""` will be given as the token consumed.
This information will be combined with its position in the input text, in the
error added to the`Partial` result.

A default value for `a` must be given, so that the parser can return something
in the event of an error and succesful recovery.

-}
forwardOrSkip : a -> List String -> x -> (String -> String -> x) -> Parser c x a -> Parser c x a
forwardOrSkip val matches noMatchProb chompedProb parser =
    PA.oneOf
        [ PA.backtrackable parser
        , chompTillToken matches noMatchProb
            |> PA.andThen
                (\res ->
                    if res.matched then
                        partialAt ( res.row, res.col )
                            val
                            (chompedProb res.discarded res.sentinal)

                    else if res.discarded == "" then
                        partialAt ( res.row, res.col )
                            val
                            noMatchProb

                    else
                        partialAt ( res.row, res.col )
                            val
                            (chompedProb res.discarded res.sentinal)
                )
        ]


{-| Not exposed, but use this as a starting point for recoverable sequences?
-}
forwardThenRetry : List String -> x -> (String -> String -> x) -> Parser c x a -> Parser c x a
forwardThenRetry matches noMatchProb chompedProb parser =
    PA.loop []
        (\warnings ->
            PA.oneOf
                [ -- Found a value, ensure any warning so far are kept.
                  PA.succeed
                    (\val ->
                        val
                            |> addWarnings warnings
                            |> PA.Done
                    )
                    |= PA.backtrackable parser
                , chompTillToken matches noMatchProb
                    |> PA.map
                        (\res ->
                            if res.discarded == "" then
                                -- Failed to make any progress, so stop.
                                Failure
                                    [ { row = res.row
                                      , col = res.col
                                      , problem = noMatchProb
                                      , contextStack = []
                                      }
                                    ]
                                    |> PA.Done

                            else
                                -- No match, but something was chomped, so try again.
                                { row = res.row
                                , col = res.col
                                , problem = chompedProb res.discarded res.sentinal
                                , contextStack = []
                                }
                                    :: warnings
                                    |> PA.Loop
                        )
                ]
        )



-- Helpers


lift : PA.Parser c x a -> Parser c x a
lift parser =
    PA.map Success parser


{-| The outcome of an attempted fast-forarding.
-}
type alias FastForward =
    { matched : Bool
    , discarded : String
    , sentinal : String
    , row : Int
    , col : Int
    }


chompTillToken : List String -> x -> PA.Parser c x FastForward
chompTillToken tokens prob =
    let
        chars =
            List.map (\val -> String.uncons val |> Maybe.map Tuple.first) tokens
                |> values

        tokenParsers =
            List.map (\val -> PA.token (PA.Token val prob) |> PA.getChompedString) tokens
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
                            if matched then
                                PA.Done
                                    { matched = matched
                                    , discarded = discarded
                                    , sentinal = sentinal
                                    , row = row
                                    , col = col
                                    }

                            else if discardStep == "" then
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
                            [ PA.succeed (\chompedString -> ( True, chompedString ))
                                |= PA.oneOf tokenParsers
                            , PA.succeed ( False, "" )
                            ]
                )

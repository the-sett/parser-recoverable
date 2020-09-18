module Parser.Recoverable exposing
    ( Parser, Outcome(..), run, DeadEnd, inContext, Token
    , int, float, number, symbol, keyword, variable, end
    , ignore, keep
    , succeed, lazy, andThen, problem
    , oneOf, map, backtrackable, commit, token
    , sequence, Trailing, loop, Step
    , spaces, lineComment, multiComment, Nestable
    , getChompedString, chompIf, chompWhile, chompUntil, chompUntilEndOr, mapChompedString
    , withIndent, getIndent
    , getPosition, getRow, getCol, getOffset, getSource
    , failOnError
    , RecoveryTactic(..), withRecovery
    )

{-|


# Parsers

@docs Parser, Outcome, try, run, DeadEnd, inContext, Token, OnError

---

**Everything past here works just like in the
[`Parser`](/packages/elm/parser/latest/Parser) module, except that `String`
arguments become `Token` arguments, and you need to provide a `Problem` for
certain scenarios.**

---


# Building Blocks

@docs int, float, number, symbol, keyword, variable, end


# Pipelines

@docs ignore, keep
@docs succeed, (|=), (|.), lazy, andThen, problem


# Branches

@docs oneOf, map, backtrackable, commit, token


# Loops

@docs sequence, Trailing, loop, Step


# Whitespace

@docs spaces, lineComment, multiComment, Nestable


# Chompers

@docs getChompedString, chompIf, chompWhile, chompUntil, chompUntilEndOr, mapChompedString


# Indentation

@docs withIndent, getIndent


# Positions

@docs getPosition, getRow, getCol, getOffset, getSource


# Error Recovery Tactics

@docs failOnError, warnOnError

-}

import Parser.Advanced as PA exposing ((|.), (|=))


{-| Describes the possible outcomes from running a parser.

    - `Success` means that the parsing completed with no syntax errors at all.
    - `Partial` means that the parsing was able to complete by recovering from
    syntax errors. The syntax errors are listed along with the parsed result.
    - `Failure` means that the parsing could not complete, so there is no parsed
    result, only a list of errors.

-}
type Outcome context problem value
    = Success value
    | Partial (List (DeadEnd context problem)) value
    | Failure (List (DeadEnd context problem))


addWarnings : List (DeadEnd c x) -> Outcome c x a -> Outcome c x a
addWarnings warnings outcome =
    case outcome of
        Success data ->
            Partial warnings data

        Partial existingWarnings data ->
            Partial (existingWarnings ++ warnings) data

        Failure existingFailures ->
            Failure (existingFailures ++ warnings)


mapOutcome : (a -> b) -> Outcome c x a -> Outcome c x b
mapOutcome fn outcome =
    case outcome of
        Success data ->
            Success (fn data)

        Partial warn data ->
            Partial warn (fn data)

        Failure err ->
            Failure err


type Parser context problem value
    = Parser
        (RecoveryTactic problem
         ->
            { pa : PA.Parser context problem (Outcome context problem value)
            , onError : RecoveryTactic problem
            }
        )


type alias Token x =
    PA.Token x


run : Parser c x a -> String -> Outcome c x a
run (Parser parserFn) input =
    let
        pa =
            parserFn Fail |> .pa
    in
    case PA.run pa input of
        Err deadEnds ->
            Failure deadEnds

        Ok outcome ->
            outcome


type alias DeadEnd context problem =
    PA.DeadEnd context problem


inContext : c -> Parser c x a -> Parser c x a
inContext ctx (Parser parserFn) =
    Parser
        (\s ->
            { pa = PA.inContext ctx (parserFn s).pa
            , onError = s
            }
        )


symbol : Token x -> Parser c x ()
symbol details =
    PA.symbol details |> parseWithRecovery ()


keyword : Token x -> Parser c x ()
keyword details =
    PA.keyword details |> parseWithRecovery ()


int =
    PA.int


float =
    PA.float


number =
    PA.number


variable =
    PA.variable


end : x -> Parser c x ()
end prob =
    PA.end prob |> parseWithRecovery ()


ignore : Parser c x ignore -> Parser c x keep -> Parser c x keep
ignore ignoreParser keepParser =
    map2 always keepParser ignoreParser


keep : Parser c x a -> Parser c x (a -> b) -> Parser c x b
keep parseArg parseFunc =
    map2 (<|) parseFunc parseArg


lazy =
    PA.lazy


problem =
    PA.problem


succeed : a -> Parser c x a
succeed x =
    Parser
        (\s ->
            { pa = PA.succeed (Success x)
            , onError = s
            }
        )


backtrackable =
    PA.backtrackable


commit =
    PA.commit


oneOf : x -> List (Parser c x a) -> Parser c x a
oneOf prob options =
    Parser
        (\s ->
            { pa =
                PA.oneOf
                    (List.foldr
                        (\(Parser tryParserFn) accum -> (tryParserFn s).pa :: accum)
                        [ PA.succeed
                            (\( row, col ) ->
                                Failure
                                    [ { row = row
                                      , col = col
                                      , problem = prob
                                      , contextStack = []
                                      }
                                    ]
                            )
                            |= PA.getPosition
                        ]
                        options
                    )
            , onError = s
            }
        )


map : (a -> b) -> Parser c x a -> Parser c x b
map fn (Parser pfun) =
    Parser
        (\s0 ->
            let
                { pa, onError } =
                    pfun s0
            in
            { pa = (mapOutcome >> PA.map) fn pa
            , onError = onError
            }
        )


map2 : (a -> b -> value) -> Parser c x a -> Parser c x b -> Parser c x value
map2 func (Parser pfunA) (Parser pfunB) =
    Parser
        (\s0 ->
            { pa =
                let
                    parserA =
                        pfunA s0
                in
                parserA.pa
                    |> PA.andThen
                        (\outcomeOfA ->
                            case outcomeOfA of
                                Success valA ->
                                    let
                                        parserB =
                                            pfunB s0
                                    in
                                    (mapOutcome >> PA.map)
                                        (func valA)
                                        parserB.pa

                                Partial warn valA ->
                                    let
                                        parserB =
                                            pfunB s0
                                    in
                                    (mapOutcome >> PA.map)
                                        (func valA)
                                        parserB.pa
                                        |> PA.map (addWarnings warn)

                                Failure err ->
                                    PA.succeed (Failure err)
                        )
            , onError = s0
            }
        )


andThen : (a -> Parser c x b) -> Parser c x a -> Parser c x b
andThen fn (Parser pfunA) =
    Parser
        (\s0 ->
            { pa =
                let
                    parserA =
                        pfunA s0
                in
                parserA.pa
                    |> PA.andThen
                        (\outcomeOfA ->
                            case outcomeOfA of
                                Success valA ->
                                    let
                                        (Parser parserB) =
                                            fn valA
                                    in
                                    (parserB s0).pa

                                Partial warn valA ->
                                    let
                                        (Parser parserB) =
                                            fn valA
                                    in
                                    (parserB s0).pa
                                        |> PA.map (addWarnings warn)

                                Failure err ->
                                    PA.succeed (Failure err)
                        )
            , onError = s0
            }
        )


token : Token x -> Parser c x ()
token tok =
    PA.token tok |> parseWithRecovery ()


sequence =
    PA.sequence


type alias Trailing =
    PA.Trailing


loop : state -> (state -> Parser c x (Step state a)) -> Parser c x a
loop state callback =
    Debug.todo "Figure out the parser return type first."


type alias Step state a =
    PA.Step state a


spaces : Parser c x ()
spaces =
    PA.spaces |> parseWithRecovery ()


lineComment =
    PA.lineComment


multiComment =
    PA.multiComment


type alias Nestable =
    PA.Nestable


getChompedString =
    PA.getChompedString


chompIf : (Char -> Bool) -> x -> Parser c x ()
chompIf fn prob =
    PA.chompIf fn prob |> parseWithRecovery ()


chompWhile : (Char -> Bool) -> Parser c x ()
chompWhile whileFn =
    PA.chompWhile whileFn |> parseWithRecovery ()


chompUntil : Token x -> Parser c x ()
chompUntil tok =
    PA.chompUntil tok |> parseWithRecovery ()


chompUntilEndOr : String -> Parser c x ()
chompUntilEndOr val =
    PA.chompUntilEndOr val |> parseWithRecovery ()


mapChompedString =
    PA.mapChompedString


withIndent =
    PA.withIndent


getIndent =
    PA.getIndent


getPosition =
    PA.getPosition


getRow =
    PA.getRow


getCol =
    PA.getCol


getOffset =
    PA.getOffset


getSource =
    PA.getSource



-- Error Recovery Tactics


type RecoveryTactic x
    = Fail
    | Warn x
    | Ignore
    | ChompForMatch (List Char) x


withRecovery : RecoveryTactic x -> Parser c x a -> Parser c x a
withRecovery tactic (Parser parser) =
    Parser (\_ -> parser tactic)


{-| Runs a `Parser.Advanced.Parser` in the normal way. If it fails, parsing
stops with an error.
-}
failOnError : PA.Parser c x a -> PA.Parser c x (Outcome c x a)
failOnError parser =
    PA.map Success parser


{-| Runs a `Parser.Advanced.Parser`. If it fails, an alternative parsing is
used, which succeeds with a `problem` as a warning. The parsing does not fail
but keeps running.
-}
warnOnError : a -> x -> PA.Parser c x a -> PA.Parser c x (Outcome c x a)
warnOnError val prob parser =
    PA.oneOf
        [ PA.map Success parser
        , partial val prob
        ]


{-| Desired outcomes:

Found the token and parsed it ok. Return data.
Did not find the token, parse it anyway.

-}
ignoreError : a -> PA.Parser c x a -> PA.Parser c x (Outcome c x a)
ignoreError val parser =
    PA.oneOf
        [ PA.map Success parser
        , PA.succeed (Success val)
        ]


{-| Desired outcomes:

Found the token and parsed it ok. Return data.
Did not find the token, but chomped for recovery ok. Warn error, no data.
Did not find the token and failed to chomp for recovery. Normal error.

-}
chompForMatchOnError : a -> List Char -> x -> PA.Parser c x a -> PA.Parser c x (Outcome c x a)
chompForMatchOnError val matches prob parser =
    PA.oneOf
        [ PA.map Success parser
        , chompTill matches prob
            |> PA.andThen
                (\( foundMatch, chompedString, pos ) ->
                    if foundMatch then
                        partialAt pos val prob

                    else
                        failureAt pos prob
                )
        ]


parseWithRecovery : a -> PA.Parser c x a -> Parser c x a
parseWithRecovery val parser =
    Parser
        (\s ->
            { pa =
                case s of
                    Fail ->
                        failOnError parser

                    Warn err ->
                        warnOnError val err parser

                    Ignore ->
                        ignoreError val parser

                    ChompForMatch matches err ->
                        chompForMatchOnError val matches err parser
            , onError = s
            }
        )



-- Helpers


chompTill : List Char -> x -> PA.Parser c x ( Bool, String, ( Int, Int ) )
chompTill chars prob =
    PA.succeed (\pos val flag -> ( flag, val, pos ))
        |= PA.getPosition
        |= (PA.chompWhile (\c -> not <| List.member (Debug.log "skipping" c) chars)
                |> PA.getChompedString
           )
        |= PA.oneOf
            [ PA.map (always True)
                (PA.chompIf (\c -> List.member (Debug.log "matched" c) chars) prob)
            , PA.succeed False
            ]


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


failure : x -> PA.Parser c x (Outcome c x a)
failure prob =
    PA.problem prob


failureAt : ( Int, Int ) -> x -> PA.Parser c x (Outcome c x a)
failureAt ( row, col ) prob =
    Failure
        [ { row = row
          , col = col
          , problem = prob
          , contextStack = []
          }
        ]
        |> PA.succeed

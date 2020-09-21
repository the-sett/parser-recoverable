module Parser.Recoverable exposing
    ( Parser, run, Outcome(..), DeadEnd, inContext
    , int, float, number, symbol, keyword, variable, end
    , ignore, keep
    , succeed, lazy, andThen, problem
    , oneOf, map, backtrackable, commit, token
    , sequence, Trailing, loop, Step
    , spaces, lineComment, multiComment, Nestable
    , getChompedString, chompIf, chompWhile, chompUntil, chompUntilEndOr, mapChompedString
    , withIndent, getIndent
    , getPosition, getRow, getCol, getOffset, getSource
    , RecoveryTactic(..), withRecovery
    )

{-|


# Parsers

@docs Parser, run, Outcome, DeadEnd, inContext, Token

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
@docs succeed, lazy, andThen, problem


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

@docs RecoveryTactic, withRecovery

-}

import Parser.Advanced as PA exposing ((|.), (|=))
import Set exposing (Set)


{-| The type of recoverable parsers.
-}
type Parser context problem value
    = Parser
        (RecoveryTactic problem
         ->
            { pa : PA.Parser context problem (Outcome context problem value)
            , onError : RecoveryTactic problem
            }
        )


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


{-| The same as in Parser.Advanced.
-}
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



-- Building Blocks


lift parser =
    Parser
        (\s ->
            { pa = parser |> PA.map Success
            , onError = s
            }
        )


int : x -> x -> Parser c x Int
int expecting invalid =
    PA.int expecting invalid |> lift


float : x -> x -> Parser c x Float
float expecting invalid =
    PA.float expecting invalid |> lift


number :
    { int : Result x (Int -> a)
    , hex : Result x (Int -> a)
    , octal : Result x (Int -> a)
    , binary : Result x (Int -> a)
    , float : Result x (Float -> a)
    , invalid : x
    , expecting : x
    }
    -> Parser c x a
number numDef =
    PA.number numDef |> lift


symbol : String -> x -> Parser c x ()
symbol match prob =
    PA.Token match prob |> PA.symbol |> parseWithRecovery ()


keyword : String -> x -> Parser c x ()
keyword match prob =
    PA.Token match prob |> PA.keyword |> parseWithRecovery ()


variable :
    { start : Char -> Bool
    , inner : Char -> Bool
    , reserved : Set String
    , expecting : x
    }
    -> Parser c x String
variable varDef =
    PA.variable varDef |> lift


end : x -> Parser c x ()
end prob =
    PA.end prob |> parseWithRecovery ()



-- Pipelines


succeed : a -> Parser c x a
succeed x =
    Parser
        (\s ->
            { pa = PA.succeed (Success x)
            , onError = s
            }
        )


keep : Parser c x a -> Parser c x (a -> b) -> Parser c x b
keep parseArg parseFunc =
    map2 (<|) parseFunc parseArg


ignore : Parser c x ignore -> Parser c x keep -> Parser c x keep
ignore ignoreParser keepParser =
    map2 always keepParser ignoreParser


lazy =
    PA.lazy


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


problem =
    PA.problem



-- Branches


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


backtrackable =
    PA.backtrackable


commit =
    PA.commit


token : String -> x -> Parser c x ()
token match prob =
    PA.Token match prob |> PA.token |> parseWithRecovery ()



-- Loops


sequence =
    PA.sequence


{-| The same as in Parser.Advanced.
-}
type alias Trailing =
    PA.Trailing


loop : state -> (state -> Parser c x (Step state a)) -> Parser c x a
loop state callback =
    Debug.todo "Figure out the parser return type first."


{-| The same as in Parser.Advanced.
-}
type alias Step state a =
    PA.Step state a



-- Whitespace


spaces : Parser c x ()
spaces =
    PA.spaces |> parseWithRecovery ()


lineComment =
    PA.lineComment


multiComment =
    PA.multiComment


{-| The same as in Parser.Advanced.
-}
type alias Nestable =
    PA.Nestable



-- Chopmers


getChompedString =
    PA.getChompedString


chompIf : (Char -> Bool) -> x -> Parser c x ()
chompIf fn prob =
    PA.chompIf fn prob |> parseWithRecovery ()


chompWhile : (Char -> Bool) -> Parser c x ()
chompWhile whileFn =
    PA.chompWhile whileFn |> parseWithRecovery ()


chompUntil : String -> x -> Parser c x ()
chompUntil match prob =
    PA.Token match prob |> PA.chompUntil |> parseWithRecovery ()


chompUntilEndOr : String -> Parser c x ()
chompUntilEndOr val =
    PA.chompUntilEndOr val |> parseWithRecovery ()


mapChompedString =
    PA.mapChompedString



-- Indentation


withIndent =
    PA.withIndent


getIndent =
    PA.getIndent



-- Positions


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


{-| Describes the possible ways the parser should act when it encounters
something that it cannot parse.

    - `Fail` stop parsing and return a `Failure` outcome.
    - `Warn` ignore the error, but add a problem and use a `Partial` outcome.
    - `Ignore` ignore the error and continue with a `Success` outcome.
    - `ChompForMatch` try chomping to find a matching character. If succesfull
    add a problem but continue with a `Partial` outcome. If this does not work
    then `Fail`.

TODO: Make the problem builders more sophisticated, by offering the chopmed
string and anything else an interactive editor might like to know. (Position
is already available in DeadEnd).

-}
type RecoveryTactic x
    = Fail
    | Warn x -- Skip
    | ChompForMatch (List Char) (String -> x)
    | ChompForMatchOrSkip (List Char) (String -> x)
    | Ignore


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
chompForMatchOnError : a -> List Char -> (String -> x) -> PA.Parser c x a -> PA.Parser c x (Outcome c x a)
chompForMatchOnError val matches prob parser =
    PA.oneOf
        [ PA.map Success parser
        , chompTill matches prob
            |> PA.andThen
                (\( foundMatch, chompedString, pos ) ->
                    if foundMatch then
                        partialAt pos val (prob chompedString)

                    else
                        failureAt pos (prob "")
                )
        ]


chompForMatchOrSkipOnError : a -> List Char -> (String -> x) -> PA.Parser c x a -> PA.Parser c x (Outcome c x a)
chompForMatchOrSkipOnError val matches prob parser =
    PA.oneOf
        [ PA.map Success parser
        , chompTill matches prob
            |> PA.andThen
                (\( foundMatch, chompedString, pos ) ->
                    if foundMatch then
                        partialAt pos val (prob chompedString)

                    else
                        partialAt pos val (prob chompedString)
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

                    ChompForMatch matches errFn ->
                        chompForMatchOnError val matches errFn parser

                    ChompForMatchOrSkip matches errFn ->
                        chompForMatchOrSkipOnError val matches errFn parser
            , onError = s
            }
        )



-- Helpers


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

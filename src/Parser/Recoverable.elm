module Parser.Recoverable exposing
    ( Parser, run, Outcome(..), DeadEnd, inContext
    , silent, skip, forward, forwardOrSkip, forwardThenRetry
    , int, float, number, symbol, keyword, variable, end
    , ignore, keep
    , succeed, lazy, andThen, problem
    , oneOf, map, backtrackable, commit, token
    , sequence, Trailing(..), loop, Step(..)
    , spaces, lineComment, multiComment, Nestable(..)
    , getChompedString, chompIf, chompWhile, chompUntil, chompUntilEndOr, mapChompedString
    , withIndent, getIndent
    , getPosition, getRow, getCol, getOffset, getSource
    )

{-|


# Parsers

@docs Parser, run, Outcome, DeadEnd, inContext


# Error Recovery Tactics

@docs silent, skip, forward, forwardOrSkip, forwardThenRetry

---

**Everything past here works just like in the
[`Parser`](/packages/elm/parser/latest/Parser) module, except for these
differences:**

    - `String` arguments become 2 arguments - a String and a `Problem`, since
    you need to define which problem to report when a String is not matched.

    - There are certain other functions which also need a `Problem` argument.

    - The `|=` and `|.` operators are only available to kernel packages. You
    need to use `|> keep` and `|> ignore` instead.

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

-}

import Parser.Advanced as PA exposing ((|.), (|=))
import Set exposing (Set)


{-| The type of recoverable parsers.
-}
type alias Parser context problem value =
    PA.Parser context problem (Outcome context problem value)


run : Parser c x a -> String -> Outcome c x a
run parser input =
    case PA.run parser input of
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
    case warnings of
        [] ->
            outcome

        _ ->
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
inContext ctx parser =
    PA.inContext ctx parser



-- Building Blocks


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
    PA.Token match prob |> PA.symbol |> lift


keyword : String -> x -> Parser c x ()
keyword match prob =
    PA.Token match prob |> PA.keyword |> lift


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
    PA.end prob |> lift



-- Pipelines


succeed : a -> Parser c x a
succeed val =
    PA.succeed (Success val)


problem : x -> Parser c x a
problem x =
    PA.problem x |> lift


keep : Parser c x a -> Parser c x (a -> b) -> Parser c x b
keep parseArg parseFunc =
    map2 (<|) parseFunc parseArg


ignore : Parser c x ignore -> Parser c x keep -> Parser c x keep
ignore ignoreParser keepParser =
    map2 always keepParser ignoreParser


lazy : (() -> Parser c x a) -> Parser c x a
lazy thunk =
    PA.lazy thunk


andThen : (a -> Parser c x b) -> Parser c x a -> Parser c x b
andThen fn parserA =
    parserA
        |> PA.andThen
            (\outcomeOfA ->
                case outcomeOfA of
                    Success valA ->
                        let
                            parserB =
                                fn valA
                        in
                        parserB

                    Partial warn valA ->
                        let
                            parserB =
                                fn valA
                        in
                        parserB
                            |> PA.map (addWarnings warn)

                    Failure err ->
                        PA.succeed (Failure err)
            )



-- Branches


oneOf : List (Parser c x a) -> Parser c x a
oneOf options =
    PA.oneOf options


map : (a -> b) -> Parser c x a -> Parser c x b
map fn parserA =
    (mapOutcome >> PA.map) fn parserA


map2 : (a -> b -> value) -> Parser c x a -> Parser c x b -> Parser c x value
map2 func parserA parserB =
    parserA
        |> PA.andThen
            (\outcomeOfA ->
                case outcomeOfA of
                    Success valA ->
                        (mapOutcome >> PA.map)
                            (func valA)
                            parserB

                    Partial warn valA ->
                        (mapOutcome >> PA.map)
                            (func valA)
                            parserB
                            |> PA.map (addWarnings warn)

                    Failure err ->
                        PA.succeed (Failure err)
            )


backtrackable : Parser c x a -> Parser c x a
backtrackable parser =
    PA.backtrackable parser


commit : a -> Parser c x a
commit val =
    PA.commit val |> lift


token : String -> x -> Parser c x ()
token match prob =
    PA.Token match prob |> PA.token |> lift



-- Loops


sequence :
    { start : ( String, x )
    , separator : ( String, x )
    , end : ( String, x )
    , spaces : Parser c x ()
    , item : Parser c x a
    , trailing : Trailing
    }
    -> Parser c x (List a)
sequence seqDef =
    let
        tokenParser ( match, prob ) =
            token match prob
    in
    sequenceEnd (tokenParser seqDef.end) seqDef.spaces seqDef.item (tokenParser seqDef.separator) seqDef.trailing
        |> ignore seqDef.spaces
        |> ignore (tokenParser seqDef.start)


sequenceEnd : Parser c x () -> Parser c x () -> Parser c x a -> Parser c x () -> Trailing -> Parser c x (List a)
sequenceEnd ender ws parseItem sep trailing =
    let
        chompRest item =
            case trailing of
                Forbidden ->
                    loop [ item ] (sequenceEndForbidden ender ws parseItem sep)

                Optional ->
                    loop [ item ] (sequenceEndOptional ender ws parseItem sep)

                Mandatory ->
                    ignorer
                        (ignore ws <|
                            ignore sep <|
                                ignore ws <|
                                    loop [ item ] (sequenceEndMandatory ws parseItem sep)
                        )
                        ender
    in
    oneOf
        [ parseItem |> andThen chompRest
        , ender |> map (\_ -> [])
        ]


sequenceEndForbidden : Parser c x () -> Parser c x () -> Parser c x a -> Parser c x () -> List a -> Parser c x (Step (List a) (List a))
sequenceEndForbidden ender ws parseItem sep revItems =
    let
        chompRest item =
            sequenceEndForbidden ender ws parseItem sep (item :: revItems)
    in
    ignore ws <|
        oneOf
            [ ignore sep <| ignore ws <| map (\item -> Loop (item :: revItems)) parseItem
            , ender |> map (\_ -> Done (List.reverse revItems))
            ]


sequenceEndOptional : Parser c x () -> Parser c x () -> Parser c x a -> Parser c x () -> List a -> Parser c x (Step (List a) (List a))
sequenceEndOptional ender ws parseItem sep revItems =
    let
        parseEnd =
            map (\_ -> Done (List.reverse revItems)) ender
    in
    ignore ws <|
        oneOf
            [ ignore sep <|
                ignore ws <|
                    oneOf
                        [ parseItem |> map (\item -> Loop (item :: revItems))
                        , parseEnd
                        ]
            , parseEnd
            ]


sequenceEndMandatory : Parser c x () -> Parser c x a -> Parser c x () -> List a -> Parser c x (Step (List a) (List a))
sequenceEndMandatory ws parseItem sep revItems =
    oneOf
        [ map (\item -> Loop (item :: revItems)) <|
            ignorer parseItem (ignorer ws (ignorer sep ws))
        , map (\_ -> Done (List.reverse revItems)) (succeed ())
        ]


ignorer : Parser c x keep -> Parser c x ignore -> Parser c x keep
ignorer keepParser ignoreParser =
    ignore ignoreParser keepParser


{-| The same as in Parser.Advanced.
-}
type Trailing
    = Forbidden
    | Optional
    | Mandatory


loop : state -> (state -> Parser c x (Step state a)) -> Parser c x a
loop state callback =
    callback state
        |> andThen
            (\nextStep ->
                case nextStep of
                    Loop nextState ->
                        loop nextState callback

                    Done val ->
                        succeed val
            )


{-| The same as in Parser.Advanced.
-}
type Step state a
    = Loop state
    | Done a



-- Whitespace


spaces : Parser c x ()
spaces =
    PA.spaces |> lift


lineComment : String -> x -> Parser c x ()
lineComment match prob =
    PA.Token match prob |> PA.lineComment |> lift


multiComment : String -> x -> String -> x -> Nestable -> Parser c x ()
multiComment open openProb close closeProb nestable =
    let
        mappedNestable =
            case nestable of
                NotNestable ->
                    PA.NotNestable

                Nestable ->
                    PA.Nestable
    in
    PA.multiComment
        (PA.Token open openProb)
        (PA.Token close closeProb)
        mappedNestable
        |> lift


{-| The same as in Parser.Advanced.
-}
type Nestable
    = NotNestable
    | Nestable



-- Chopmers


getChompedString : Parser c x a -> Parser c x String
getChompedString parser =
    mapChompedString always parser


chompIf : (Char -> Bool) -> x -> Parser c x ()
chompIf fn prob =
    PA.chompIf fn prob |> lift


chompWhile : (Char -> Bool) -> Parser c x ()
chompWhile whileFn =
    PA.chompWhile whileFn |> lift


chompUntil : String -> x -> Parser c x ()
chompUntil match prob =
    PA.Token match prob |> PA.chompUntil |> lift


chompUntilEndOr : String -> Parser c x ()
chompUntilEndOr val =
    PA.chompUntilEndOr val |> lift


mapChompedString : (String -> a -> b) -> Parser c x a -> Parser c x b
mapChompedString func parser =
    --PA.mapChompedString func parser
    Debug.todo "mapChompedString"



-- Indentation


getIndent : Parser c x Int
getIndent =
    PA.getIndent |> lift


withIndent : Int -> Parser c x a -> Parser c x a
withIndent newIndent parser =
    PA.withIndent newIndent parser



-- Positions


getPosition : Parser c x ( Int, Int )
getPosition =
    PA.getPosition |> lift


getRow : Parser c x Int
getRow =
    PA.getRow |> lift


getCol : Parser c x Int
getCol =
    PA.getCol |> lift


getOffset : Parser c x Int
getOffset =
    PA.getOffset |> lift


getSource : Parser c x String
getSource =
    PA.getSource |> lift



-- Error Recovery Tactics


silent : a -> PA.Parser c x a -> PA.Parser c x (Outcome c x a)
silent val parser =
    PA.oneOf
        [ PA.map Success parser
        , PA.succeed (Success val)
        ]


skip : a -> x -> PA.Parser c x a -> PA.Parser c x (Outcome c x a)
skip val prob parser =
    PA.oneOf
        [ PA.map Success parser
        , partial val prob
        ]


forward : a -> List Char -> x -> (String -> x) -> PA.Parser c x a -> PA.Parser c x (Outcome c x a)
forward val matches noMatchProb chompedProb parser =
    PA.oneOf
        [ PA.map Success parser
        , chompTill matches noMatchProb
            |> PA.andThen
                (\( foundMatch, chompedString, pos ) ->
                    if foundMatch then
                        partialAt pos val (chompedProb chompedString)

                    else
                        failureAt pos noMatchProb
                )
        ]


forwardOrSkip : a -> List Char -> x -> (String -> x) -> PA.Parser c x a -> PA.Parser c x (Outcome c x a)
forwardOrSkip val matches noMatchProb chompedProb parser =
    PA.oneOf
        [ PA.map Success parser
        , chompTill matches noMatchProb
            |> PA.andThen
                (\( foundMatch, chompedString, pos ) ->
                    if foundMatch then
                        partialAt pos val (chompedProb chompedString)

                    else
                        partialAt pos val noMatchProb
                )
        ]


forwardThenRetry : List Char -> x -> (String -> x) -> Parser c x a -> Parser c x a
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
                , chompTill matches noMatchProb
                    |> PA.map
                        (\( foundMatch, chompedString, ( row, col ) ) ->
                            if chompedString == "" then
                                -- Failed to make any progress, so stop.
                                Failure
                                    [ { row = row
                                      , col = col
                                      , problem = noMatchProb
                                      , contextStack = []
                                      }
                                    ]
                                    |> PA.Done

                            else
                                -- No match, but something was chomped, so try again.
                                { row = row
                                , col = col
                                , problem = chompedProb chompedString
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

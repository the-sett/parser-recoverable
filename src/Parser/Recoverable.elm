module Parser.Recoverable exposing
    ( Parser, run, Outcome(..)
    , DeadEnd, inContext
    , int, float, number, symbol, keyword, variable, token, end
    , ignore, keep
    , succeed, lazy, andThen, problem
    , oneOf, map, backtrackable, commit
    , sequence, Trailing(..), loop, Step(..)
    , spaces, lineComment, multiComment, Nestable(..)
    , getChompedString, chompIf, chompWhile, chompUntil, chompUntilEndOr, mapChompedString
    , withIndent, getIndent
    , getPosition, getRow, getCol, getOffset, getSource
    )

{-| `Parser.Recoverable` encodes parsers that can have `Partial` result when
there is an error, but the parsing is able to recover and continue and still
produce a result.

This `Parser.Recoverable` module does not do any automatic error recovery, its
functions behave the same way as those in `Parser.Advanced`.


# Parsers

@docs Parser, run, Outcome

---

**Everything past here works just like in the
[`Parser`](/packages/elm/parser/latest/Parser) module, except for these
differences:**

  - `String` arguments become 2 arguments - a `String` and a `problem`, since
    you need to define which problem to report when a `String` is not matched.

  - There are certain other functions which also need a `problem` argument.

  - The `|=` and `|.` operators are only available to kernel packages, and this
    package cannot export them. You need to use `|> keep` and `|> ignore` instead.

  - When combining parser with function such as `keep` or `andThen`, if one of
    the parsers fails, then parsing will stop with `Failure`. If no parser fails,
    but one of them yields a `Partial` result, parsing will continue with the
    errors carrying forward. Due to this, parsers that have recovered succesfully
    from errors will keep running.

---


# Basics

@docs DeadEnd, inContext


# Building Blocks

@docs int, float, number, symbol, keyword, variable, token, end


# Pipelines

@docs ignore, keep
@docs succeed, lazy, andThen, problem


# Branches

@docs oneOf, map, backtrackable, commit


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


{-| Runs the parser. This differs from `Parser.run` in that it produces an
`Outcome` instead of a `Result`. This allows for a `Partial` result when the
parser recovers from an error. In this case the parser will still produce a
result, but will also list the errors it encountered and was able to recover
from.
-}
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


{-| The same as `Parser.Advanced.DeadEnd`.
-}
type alias DeadEnd context problem =
    PA.DeadEnd context problem


{-| Just like `Parser.inContext`.
-}
inContext : c -> Parser c x a -> Parser c x a
inContext ctx parser =
    PA.inContext ctx parser



-- Building Blocks


{-| Just like `Parser.int` where you have to handle negation
yourself. The only difference is that you provide a two potential problems:

    int : x -> x -> Parser c x Int
    int expecting invalid =
        number
            { int = Ok identity
            , hex = Err invalid
            , octal = Err invalid
            , binary = Err invalid
            , float = Err invalid
            , invalid = invalid
            , expecting = expecting
            }

You can use problems like `ExpectingInt` and `InvalidNumber`.

-}
int : x -> x -> Parser c x Int
int expecting invalid =
    PA.int expecting invalid |> lift


{-| Just like `Parser.float` where you have to handle negation yourself. The
only difference is that you provide a two potential problems:

    float : x -> x -> Parser c x Float
    float expecting invalid =
        number
            { int = Ok toFloat
            , hex = Err invalid
            , octal = Err invalid
            , binary = Err invalid
            , float = Ok identity
            , invalid = invalid
            , expecting = expecting
            }

You can use problems like `ExpectingFloat` and `InvalidNumber`.

-}
float : x -> x -> Parser c x Float
float expecting invalid =
    PA.float expecting invalid |> lift


{-| Just like `Parser.number` where you have to handle negation yourself. The
only difference is that you provide all the potential problems.
-}
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


{-| Just like `Parser.symbol` except you also provide a custom problem.

    comma : Parser Context Problem ()
    comma =
        symbol "," ExpectingComma

-}
symbol : String -> x -> Parser c x ()
symbol match prob =
    PA.Token match prob |> PA.symbol |> lift


{-| Just like Parser.keyword except you provide a custom problem.

    let_ : Parser Context Problem ()
    let_ =
        symbol "let" ExpectingLet

Note that this would fail to chomp `letter` because of the subsequent
characters. Use `token` if you do not want that last letter check.

-}
keyword : String -> x -> Parser c x ()
keyword match prob =
    PA.Token match prob |> PA.keyword |> lift


{-| Just like `Parser.variable` except you specify a custom problem.
-}
variable :
    { start : Char -> Bool
    , inner : Char -> Bool
    , reserved : Set String
    , expecting : x
    }
    -> Parser c x String
variable varDef =
    PA.variable varDef |> lift


{-| Just like `Parser.end`except you provide the problem that arises when the
parser is not at the end of the input.
-}
end : x -> Parser c x ()
end prob =
    PA.end prob |> lift



-- Pipelines


{-| Just like `Parser.succeed`.
-}
succeed : a -> Parser c x a
succeed val =
    PA.succeed (Success val)


{-| Just like `Parser.problem` except you provide a custom problem.
-}
problem : x -> Parser c x a
problem x =
    PA.problem x |> lift


{-| This is a flipped version of `|=`

If either parser fails, the result of this will fail. If either parser produces
a `Partial` result but not a `Failure`, the parsing will continue and carry
forward any errors and produce a `Partial` result.

-}
keep : Parser c x a -> Parser c x (a -> b) -> Parser c x b
keep parseArg parseFunc =
    map2 (<|) parseFunc parseArg


{-| This is a flipped version of `|.`

If either parser fails, the result of this will fail. If either parser produces
a `Partial` result but not a `Failure`, the parsing will continue and carry
forward any errors and produce a `Partial` result.

-}
ignore : Parser c x ignore -> Parser c x keep -> Parser c x keep
ignore ignoreParser keepParser =
    map2 always keepParser ignoreParser


{-| Just like `Parser.lazy`.
-}
lazy : (() -> Parser c x a) -> Parser c x a
lazy thunk =
    PA.lazy thunk


{-| Just like `Parser.andThen`, except that parsing will continue if the parser
produces a `Partial` result. Any errors attached to a `Partial` result will
always be carried forward by the parser, so the overall result at the end will
be `Partial`.
-}
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


{-| Just like `Parser.oneOf`.
-}
oneOf : List (Parser c x a) -> Parser c x a
oneOf options =
    -- If the parser succeeds with a Failure, which will not trigger trying other
    -- options in a PA.oneOf.
    --
    -- To get around this, there may need to be an internal `Outcome` type:
    --
    -- type OutcomeInternal c x a
    --     = SuccessInternal a
    --     | PartialInternal (List (DeadEnd c x)) a
    --     | FailInternal x
    --
    -- That would allow single failures to be promoted to real PA.problems.
    --
    -- Or just drop FailInternal and always use PA.problem? That would simplify
    -- internal logic on `map` and `andThen` and so on?
    PA.oneOf options


{-| Just like `Parser.map`, except that parsing will continue if the parser
produces a `Partial` result. Any errors attached to a `Partial` result will
always be carried forward by the parser, so the overall result at the end will
be `Partial`.
-}
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


{-| Just like `Parser.backtrackable`.
-}
backtrackable : Parser c x a -> Parser c x a
backtrackable parser =
    PA.backtrackable parser


{-| Just like `Parser.commit`.
-}
commit : a -> Parser c x a
commit val =
    PA.commit val |> lift


{-| Just like `Parser.token` except you provide a custom problem.
-}
token : String -> x -> Parser c x ()
token match prob =
    PA.Token match prob |> PA.token |> lift



-- Loops


{-| Just like `Parser.sequence`.
-}
sequence :
    { start : String
    , startProb : x
    , separator : String
    , separatorProb : x
    , end : String
    , endProb : x
    , spaces : Parser c x ()
    , item : Parser c x a
    , trailing : Trailing
    }
    -> Parser c x (List a)
sequence seq =
    succeed identity
        |> ignore (token seq.start seq.startProb)
        |> ignore seq.spaces
        |> keep
            (sequenceEnd
                (token seq.end seq.endProb)
                seq.spaces
                seq.item
                (token seq.separator seq.separatorProb)
                seq.trailing
            )


sequenceEnd :
    Parser c x ()
    -> Parser c x ()
    -> Parser c x a
    -> Parser c x ()
    -> Trailing
    -> Parser c x (List a)
sequenceEnd ender ws parseItem sep trailing =
    let
        chompRest item =
            case trailing of
                Forbidden ->
                    loop [ item ] (sequenceEndForbidden ender ws parseItem sep)

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
    -> List a
    -> Parser c x (Step (List a) (List a))
sequenceEndForbidden ender ws parseItem sep revItems =
    succeed identity
        |> ignore ws
        |> keep
            (oneOf
                [ succeed identity
                    |> ignore sep
                    |> ignore ws
                    |> keep parseItem
                    |> map (\item -> Loop (item :: revItems))
                , ender
                    |> map (\_ -> Done (List.reverse revItems))
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
            |> map (\_ -> Done (List.reverse revItems))
        ]


{-| Just like `Parser.Trailing`.
-}
type Trailing
    = Forbidden
    | Optional
    | Mandatory


{-| Just like `Parser.loop`.
-}
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


{-| Just like `Parser.Step`.
-}
type Step state a
    = Loop state
    | Done a



-- Whitespace


{-| Just like `Parser.x`.
-}
spaces : Parser c x ()
spaces =
    PA.spaces |> lift


{-| Just like `Parser.x`.
-}
lineComment : String -> x -> Parser c x ()
lineComment match prob =
    PA.Token match prob |> PA.lineComment |> lift


{-| Just like `Parser.x`.
-}
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


{-| Just like `Parser.x`.
-}
type Nestable
    = NotNestable
    | Nestable



-- Chompers


{-| Just like `Parser.getChompedString`.
-}
getChompedString : Parser c x a -> Parser c x String
getChompedString parser =
    mapChompedString always parser


{-| Just like `Parser.chompIf`.
-}
chompIf : (Char -> Bool) -> x -> Parser c x ()
chompIf fn prob =
    PA.chompIf fn prob |> lift


{-| Just like `Parser.chompWhile`.
-}
chompWhile : (Char -> Bool) -> Parser c x ()
chompWhile whileFn =
    PA.chompWhile whileFn |> lift


{-| Just like `Parser.chompUntil`.
-}
chompUntil : String -> x -> Parser c x ()
chompUntil match prob =
    PA.Token match prob |> PA.chompUntil |> lift


{-| Just like `Parser.chompUntilEndOr`.
-}
chompUntilEndOr : String -> Parser c x ()
chompUntilEndOr val =
    PA.chompUntilEndOr val |> lift


{-| Just like `Parser.mapChompedString`.
-}
mapChompedString : (String -> a -> b) -> Parser c x a -> Parser c x b
mapChompedString func parser =
    PA.mapChompedString
        (\s valA ->
            case valA of
                Success val ->
                    func s val |> Success

                err ->
                    err |> mapOutcome (func "")
        )
        parser



-- Indentation


{-| Just like `Parser.getIndent`.
-}
getIndent : Parser c x Int
getIndent =
    PA.getIndent |> lift


{-| Just like `Parser.withIndent`.
-}
withIndent : Int -> Parser c x a -> Parser c x a
withIndent newIndent parser =
    PA.withIndent newIndent parser



-- Positions


{-| Just like `Parser.getPosition`.
-}
getPosition : Parser c x ( Int, Int )
getPosition =
    PA.getPosition |> lift


{-| Just like `Parser.getRow`.
-}
getRow : Parser c x Int
getRow =
    PA.getRow |> lift


{-| Just like `Parser.getCol`.
-}
getCol : Parser c x Int
getCol =
    PA.getCol |> lift


{-| Just like `Parser.getOffset`.
-}
getOffset : Parser c x Int
getOffset =
    PA.getOffset |> lift


{-| Just like `Parser.getSource`.
-}
getSource : Parser c x String
getSource =
    PA.getSource |> lift



-- Helpers


lift : PA.Parser c x a -> Parser c x a
lift parser =
    PA.map Success parser

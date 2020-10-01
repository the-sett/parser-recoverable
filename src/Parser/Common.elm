module Parser.Common exposing
    ( addWarnings
    , failure
    , partial
    , partialAt
    , values
    )

{-| Some common functions used accross this package.
-}

import Parser.Advanced as PA exposing ((|.), (|=))
import Parser.Recoverable as PR exposing (DeadEnd, Outcome(..))


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

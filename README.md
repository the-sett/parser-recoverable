**Contacts for Support**
- @rupertlssmith on https://elmlang.slack.com
- @rupert on https://discourse.elm-lang.org

# parser-recoverable

`the-sett/parser-recoverable` is an extension to `elm/parser` that helps recovering from
parsing errors. It is designed as a drop-in replacement for `elm/parser` by following
its API closely, except for a few minor changes. The aim is to help create parsers that
will tolerate syntax errors and get back on track and allow parsing to continue. In this
case a parser may still produce a result, but may also produce some errors.

# Parsers that produce Result and Errors

The type signature of the `Parser` and its `run` function are:

```elm
type alias Parser context problem value =
    Parser.Advanced.Parser context problem (Outcome context problem value)

run : Parser c x a -> String -> Outcome c x a

type Outcome context problem value
    = Success value
    | Partial (List (DeadEnd context problem)) value
    | Failure (List (DeadEnd context problem))
```

`Outcome` describes the possible outcomes from running a parser:

* `Success` means that the parsing completed with no syntax errors at all.

* `Partial` means that the parsing was able to complete by recovering from
syntax errors. The syntax errors are listed along with the parsed result.

* `Failure` means that the parsing could not complete, so there is no parsed
result, only a list of errors.

# Recovery Tactics

`Parser.Recoverable.Tactics` provides some simple recovery tactics that can
be manually added to parsing code.

The aim is to get a parser back into a position where it can continue running,
not to attempt to correct individual tokens. For example, if the parser was
expecting an integer but did not get one, recovering by inserting a default
value such as `0` could work, but it will likely be better if the parser skips
ahead until it finds some place it can safely restart from, such as the next `;`,
and report the whole expression with the missing integer value as an error.

Recovery tactics are always associated with `Parser`s, not tokens. Often a
recovery tactic will be associated with a larger piece of parsing logic, and
not just an individual `Parser` building block such as `symbol` or `keyword`.

The available recovery tactics are:

* `optional` - This tactic may be associated with a small parser, such as
`symbol`. If the expected thing is missing from the input, it will be silently
skipped.

    The `examples/src/OptionalCommas.elm` example demonstrates this.

* `skip` - This tactic may be associated with a small parser, such as
`symbol`. If the expected thing is missing from the input, it will be skipped
over but an error will also be added to a `Partial` outcome.

    The `examples/src/WarnCommas.elm` example demonstrates this.

* `forward` - This tactic is usually associated with a larger piece of parsing
logic. When it encounters an error, it will attempt to fast-forward to one of a
list of *sentinal* tokens, consume that token, report the skipped text as an
error added to a `Partial` result, then resume parsing from that point.

* `forwardOrSkip` - This tactic first tries to do a `forward` and if that
fails, does a `skip` after the skipped text. The parser will then continue
at that point. Often the `end` token will have been reached after a failed
forward so parsing will finish there by skipping the missing token.

    The `examples/src/ListOfInts.elm` example demonstrates this.

# Recoverable Sequences

`Parser.Recoverable.Sequence` provides a recoverable version of `sequence`,
that will allow errors in the sequence items, and will fast forward to the
next separator or end token to recover.

The `examples/src/ArrayOfInts.elm` example demonstrates this, and lets you
experiment with what happens with different options for the trailing separator
token (`Forbidden`, `Optional` or `Mandatory`).

# Use Cases

A recoverable parser can be useful for a compiler parser, as it may allow more
errors to be found in source code than might otherwise be found; the error
reporting could be more complete. Normally, code with syntax errors should be
rejected by a compiler as correct syntax is a quality gateway on the code.

A more appropriate use case might be for writing an interactive editor. When
code is in the act of being edited it will pass through many states where the
syntax is not correct. Source code analysis could still be run, and such an
editor might be able to make auto complete suggestions, even when the code is
not syntactically complete.

# Discussion on Elm Discourse

Relevant discussion on Elm Discourse is here:

[https://discourse.elm-lang.org/t/parsers-with-error-recovery/6262](https://discourse.elm-lang.org/t/parsers-with-error-recovery/6262)

Thanks to Matthew Griffith for supplying some tolerant parsing code, which I
took as a starting point for developing this package.

**Contacts for Support**
- @rupertlssmith on https://elmlang.slack.com
- @rupert on https://discourse.elm-lang.org

# parser-recoverable

`the-sett/parser-recoverable` is an extension to `elm/parser` that helps recovering from
parsing errors. It is designed as a drop-in replacement for `elm/parser` by following
its API closely, except for a few minor changes. The aim is to help create parsers that
will tolerate syntax errors and get back on track and allow parsing to continue. In this
case a parser may still produce a result, but may also produce some errors.

# Getting Back on Track and Limitations

The main idea of this package is to implement some recovery tactics that may be able
to get a parser back on track when it encounters syntax errors.

Limitated by implicit rather than explicit grammar. An explicit grammar and parser generator
tool might be able to do better, because it could figure out ways in which to complete the
grammar by skipping over or inserting tokens, until it finds the shortest sequence of edits
that results in a succesfull parsing. The tactics implemented here are limited to skipping
over in search of sentinal tokens, or trying a sentinal token immediately, but not searching
for combinations of these actions that might also work.

# What Does a Recoverable Parser Look Like?

# The Recovery Tactics in Detail

# Use Cases

A recoverable parser can be useful for a compiler parser, in that it may allow more
errors to be found in some source code than might otherwise be found. The error reporting
could be more complete. Generally, code with syntax errors should be rejected by a compiler
as correct syntax is a quality gateway on the code.

A more appropriate use case might be for writing an interactive editor. When code is in
the act of being edited it will pass through many steps where the syntax is not correct.
Source code analysis could still be run, and such an editor might be able to make auto
complete suggestions, even when the code is not syntactically complete.


# Discussion on Elm Discourse

Relevant discussion on Elm Discourse is here:

[https://discourse.elm-lang.org/t/parsers-with-error-recovery/6262](https://discourse.elm-lang.org/t/parsers-with-error-recovery/6262)

Thanks to Matthew Griffith for supplying some tolerant parsing code, which I took as a
starting point for developing this package.

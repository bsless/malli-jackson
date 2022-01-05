# io.github.bsless/malli-jackson

Emit malli schemas for classes annotated with Jackson annotations

## Why

Ever tried using Apache Druid's HTTP API? While it's documented in HTML,
there is no official schema published. Therefor, your only way of
ensuring your queries are correct before sending them is crossing your
fingers, closing your eyes, and hoping for the best.

If we have a schema, we can validate, coerce, or do other interesting
things.

## Status

Experimental, pre-alpha, etc. Feedback and PRs welcome.

## Usage

Call `io.github.bsless.malli-jackson/parse+emit` with a class *name* and
get back a registry.

The registry might contain unresolved classes, so it's up to you to extend it.

## License

Copyright © 2022 Ben Sless

Distributed under the Eclipse Public License version 1.0.

# automata

A clojure library that implements regular expressions using the classic
to NFA to DFA approach.

This library is meant for learning purposes only, do not use in
production.

## Usage

Import using `(use automata.core)`

You can define regular expressions using the `regex-nfa` and `regex` macros.

Example:
```clojure
(regex-nfa | "0" (* "1") "0"
;; matches strings that start with 0, end with 0 and have 0 or more "1"s in between
```

## License

Copyright © 2015 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

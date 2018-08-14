# Feynman: a programming language for units of measure

## Usage

Using [Leiningen](https://leiningen.org/), run:

    $ lein run [args] [file]

If file is given, for each statement in the file, print the following:
* The statement itself
* Inferred type of statement
* Generated code

The args can modify what is printed:
* --no-input    - Do not print the input
* --no-types    - Do not print type inference info
* --no-generate - Do not print generated clojure code
* --eval        - Evaluate the generated code"

If no file is given, interactive session is started.

## Other commands

Run a clojure REPL:

    $ lein repl

Run tests:

    $ lein test

Generate jarfile with:

    $ lein uberjar

Which can be executed:

    $ java -jar target/uberjar/feynman-0.1.0-SNAPSHOT-standalone.jar [args]


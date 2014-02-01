# hack-clj

A Clojure application that compiles Hack assembly (.asm) files into Hack
binary (.hack) files, following chapter 6 of the [Nand2Tetris](http://www.nand2tetris.org)
course.

## Limitations

Many!

* Ignores @variables
* Ignores (LOOPS)
* Ignores any line that contains `//`, no matter where it appears

## Requirements

The same as [Leiningen](http://leiningen.org). Also, [Leiningen](http://leiningen.org).

## Usage

To compile `Foo.asm` from the `hack-clj` directory, just type `lein run path/to/Foo.asm`. 

## License

Copyright © 2014 Justin Holguin.

Distributed under the Eclipse Public License version 1.0.

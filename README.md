# hack-clj

A Clojure application that compiles Hack assembly (.asm) files into Hack
binary (.hack) files, following chapter 6 of the [Nand2Tetris](http://www.nand2tetris.org)
course.

Hack-clj produces code that is identical to the reference assembler from [Nand2Tetris](http://www.nand2tetris.org). It doesn't implement any additional features or functionality.

## Limitations

File handling is minimalistic, to say the least. Compiled programs are placed exactly where the original was, according to the path passed to hack-clj on the command line. If you run hack-clj from another directory, you should always give it the full path to the source file.

There is also no syntax/error checking during compilation (yet). Hack-clj will happily take your garbage in and give you garbage back out.

## Requirements

The same as [Leiningen](http://leiningen.org). Also, [Leiningen](http://leiningen.org).

## Usage

To compile `Foo.asm` from the `hack-clj` directory, just type `lein run path/to/Foo.asm`. To build a standalone jar for hack-clj, type `lein uberjar`.

## License

Copyright © 2014 Justin Holguin.

Distributed under the Eclipse Public License version 1.0.

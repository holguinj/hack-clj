# hack-clj



A Clojure application based on the tools described in the [Nand2Tetris](http://www.nand2tetris.org)
course. It can compile Hack VM code (".vm") into assembly (".asm"), and assembly files into Hack binary (".hack").

As an assembler, Hack-clj produces code that is identical to the reference assembler from [Nand2Tetris](http://www.nand2tetris.org).
It doesn't implement any additional features or functionality.

## Limitations

File handling is a bit bare. Compiled programs are placed exactly where the original was.

There isn't much syntax/error checking during compilation (yet). Hack-clj will usually just take your garbage in and give you garbage back out.

## Requirements

To build Hack-clj, you'll need [Leiningen](http://www.leiningen.org).

## Usage

You can run Hack-clj from the command line by changing into the `hack-clj` directory and typing `lein run`, followed by the source file/directory that you'd like to compile. Hack-clj will search directories for files ending in ".vm" and combine them into a single ".asm" output file, as per the specifications for the Hack virtual machine language. If the input file ends in ".asm", Hack-clj will compile it into a ".hack" file in the same directory.

To build a standalone jar for hack-clj, type `lein uberjar`. The jar file will be in the `target` directory.

## License

Copyright © 2014 Justin Holguin.

Distributed under the Eclipse Public License version 1.0.

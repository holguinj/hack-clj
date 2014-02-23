# hack-clj

A Clojure application that compiles Hack assembly (.asm) files into Hack
binary (.hack) files, following chapter 6 of the [Nand2Tetris](http://www.nand2tetris.org)
course. 

Hack-clj produces code that is identical to the reference assembler from [Nand2Tetris](http://www.nand2tetris.org).
It doesn't implement any additional features or functionality.

This branch also compiles Hack virtual machine instruction (.vm) files into Hack assembly (.asm) files,
although it currently supports only stack arithmetic and memory access commands. 

I'm currently working on implementing the '.vm' translator from chapter 7 the course as part of this program. Switch to the 'vm' branch to have a look, but it isn't functional yet.

## Limitations

File handling is minimalistic, to say the least. Compiled programs are placed exactly where the original was,
according to the path passed to hack-clj on the command line. If you run hack-clj from another directory,
you should always give it the full path to the source file.

There is also no syntax/error checking during compilation (yet). Hack-clj will happily take your garbage in
and give you garbage back out.

## Requirements

The same as [Leiningen](http://leiningen.org). Also, [Leiningen](http://leiningen.org).

## Usage

To compile `Foo.asm` from the `hack-clj` directory, just type `lein run path/to/Foo.asm`. To build a standalone jar for hack-clj, type `lein uberjar`.

## License

Copyright © 2014 Justin Holguin.

Distributed under the Eclipse Public License version 1.0.

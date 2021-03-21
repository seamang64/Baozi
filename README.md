# Baozi
Keiko compiler for the Baozi Language


# Installation
Install Ocaml: https://ocaml.org/

Install Dune: https://dune.build/

Run `build` in the root directory to build the compiler

Run `compile <filename>` to compile a particular file.
  this will produce a file `a.k` containing the Keiko code.
  
  
In order to run the keiko code, you will need Mike Spivey's Keiko interpreter.

Easier way to get that is to run `hg clone http://spivey.oriel.ox.ac.uk/hg/compilers`

Run `make` in every `compilers/keiko`, `compilers/lib` and `compilers/ppc`

Copy 'kompile' and `a.k` to `compilers`

Copy 'lib/baozi.k` to `compilers\keiko`

Run `kompile`. This will produce `a.out`

Run `a.out` to generate the output from your program

If this doesn't work, or you have any questions, please do not hesitate to contact me.

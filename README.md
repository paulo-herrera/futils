# INTRODUCTION

FORTRAN is one of the oldest programming languages still in use because it 
excels at handling numerical data and computations. It is also well-known for 
the high performance that can be achieved by compilers.
However, because of its long history, it is known for including what are 
considered today *poor programming practices* that make difficult to write
clear and easy to debug code. Furthemore, the original language lack flexibility
to write general use libraries for handling parts of programs that deal with 
aspects beyond strict numerical computations.

Since FORTRAN 90, the language has received changes that have made possible
to use *modern* and *safer* programming style. This is the main motivation for
**FUTILS**, which attempt to contribute small modules of code written in modern
FORTRAN that are easier and safer to use and at the same time clear and robust.  

# EXAMPLES (TESTS)

The source directory contains .f90 files that implement the available modules, while
the tests directory includes programs that use those modules and additinal data files.

To compile and use those examples, it should be enough to type in the source directory:

1. `make`
2. `make run`

# DOCUMENTATION

The examples should provide enough documentation to start using the library. Morever,
the small size and self-explanatory names of types and subroutines should make
its use straightforward.

# BUG REPORTING

Please use the bug tracking system in GitHub.

# CONTACT

If you have any issues, feel free to contact me at *paulo.herrera.ricci at gmail.com*.

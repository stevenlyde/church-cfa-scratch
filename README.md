church-cfa-scratch
==================

This project contains scratch implementations for performing a control-flow analysis of 
Church programs. It currently does not support every feature provided in Church.

To run the analysis you will need Racket. The file analyze.rkt will read a program from
standard in and write two files: state-space.gv and state-space.txt. Graphviz can then be
used to convert state-space.gv into a diagram of the abstract state space.

The first step in the analysis is to perform the desugarings, taken from the Bher 
compiler. The second step performs further desugarings. The third step converts the 
program into A-Normal Form (ANF). The last step runs 0CFA.

There is also another program that will generate a Bayesian network in s-expression from
a simple Church program. It does not perform any control-flow analysis, but uses simple 
pattern matching.

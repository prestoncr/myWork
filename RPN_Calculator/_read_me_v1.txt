Christopher R Preston, Assignment2 RPN in Ocaml COP4020 Dr. Dobra Spring 2019

to compile:
ocamlc Str.cma -o RPNv1 RPN_v1.ml

to run: 
ocamlrun RPNv1

program will pause and wait for user to input an RPN expression;

can also pipe in a text file with a single expression and 
no following whitespace by running ocamlrun RPN < input.txt

known bug - will not reject input with too many numbers, will reject all other bad input

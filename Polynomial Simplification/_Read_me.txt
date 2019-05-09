This program written in Ocaml simplifies polynomials
code is defined in poly.ml, see tests folder for test cases


Assignment 4 COP4020 Spring 19 Dr. Alin Dobra
Work Completed by: Christopher Preston

Running the tests is as simple as 
make tests

p1-p6.in are provided to showcase all working functionallity 
Each test case shows something a little more complex than the test
before it; please look at the .in files and compare to the .out files
generated to see what work has been completed for this assignment.

To run some more test cases just run "make clean" then alter
any of the p.in files or make more of them
to clean up everything first

Known Bugs:
Distributing polynomials across other polynomials does not work

For ease of comparison here is the manually calcuated result from each .in files

p1 = 8x^3 + 3x^2 + 12
p2 = 24x^10
p3 = 21x^3
p4 = 8x^5 + 6x^4 + 6x
p5 = 27x^6
p6 = 9x^6 + 2x^2 + 5
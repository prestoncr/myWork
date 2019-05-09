README

This project utilizes Java and Antlr4 to recreate BC (basic calculator)

Project Christopher R Preston
this zip archive should include: Calculator.g4, tests folder(with 6 .bc files for testing)

To create the calculator first execute:

antlr4 Calculator.g4
javac Calculator*.java

To test run:

grun Calculator exprList /filepath
Example:  grun Calculator exprList tests/basicexp.bc

Notes: Each test file displays different functionallity for each bullet point in the assignment:
- Comments: /* ... */
- Basic expressions with variables
- Boolean Expressions
- Precedence (no seperate file, all files test this)
- Special Expression: read and sqrt
- Statements: expressions (print value on the screen when executed), print expressions
- Math library functions: s, c, l, e (no need for a and j)

"readandsqrt.bc" :
read() and sqrt() test file has comments to instruct what to enter because it 
requires user input

read() will continually loop until valid input is entered, it can only accept a number.
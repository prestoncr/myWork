:- begin_tests(typeInf).
:- include(typeInf). 
% Work Completed by Christopher R Preston and Joseph Bozarth

/* Note: when writing tests keep in mind that 
    the use of of global variable and function definitions
    define facts for gvar() predicate. Either test
    directy infer() predicate or call
    delegeGVars() predicate to clean up gvar().
*/
% tests for typeExp
test(typeExp_iplus) :- 
    typeExp(iplus(int,int), int).

% this test should fail
test(typeExp_iplus_F, [fail]) :-
    typeExp(iplus(int, int), float).

test(typeExp_iplus_T, [true(T == int)]) :-
    typeExp(iplus(int, int), T).

% NOTE: use nondet as option to test if the test is nondeterministic

% test for statement with state cleaning
test(typeStatement_gvar, [nondet, true(T == int)]) :- % should succeed with T=int
    deleteGVars(), /* clean up variables */
    typeStatement(gvLet(v, T, iplus(X, Y)), unit),
    assertion(X == int), assertion( Y == int), % make sure the types are int
    gvar(v, int). % make sure the global variable is defined

% same test as above but with infer 
test(infer_gvar, [nondet]) :-
    infer([gvLet(v, T, iplus(X, Y))], unit),
    assertion(T==int), assertion(X==int), assertion(Y=int),
    gvar(v,int).

% test custom function with mocked definition
test(mockedFct, [nondet]) :-
    deleteGVars(), % clean up variables since we cannot use infer
    asserta(gvar(my_fct, [int, float])), % add my_fct(int)-> float to the gloval variables
    typeExp(my_fct(X), T), % infer type of expression using or function
    assertion(X==int), assertion(T==float). % make sure the types infered are correct

%end of Dobra's tests

%Start of our tests


%Testing User Defined Functions

% User defined function, Test 1
% this creates new_func with parameters int, float and return type float [int,float,float]
% function return is the last expression, fplus is of type float, thus T == float
test(infer_defFunc, [nondet]) :-
    infer([defFunc(new_func, [int,float], T,  [iplus(int,int), fplus(float,float)])], unit),
    assertion(T == float),
    gvar(new_func, [int,float,float]).


% User defined function, Test 2
% this creates new_func2 with parameters bool, float and return type int [bool,float,int]
% function return is the last expression, fToInt is of type int, thus T == int
test(infer_defFunc, [nondet]) :-
    infer([defFunc(new_func2, [bool, float], T,  [op_and(bool,bool), fToInt(float)])], unit),
    assertion(T == int),
    gvar(new_func2, [bool, float, int]).


% User defined function, Test 3 - should fail because the return type is float but
% the assertion is indicating the function was int->int->int
test(infer_defFunc, [fail]) :-
    infer([defFunc(new_func3, [int, int], T,  [imult(int,int), fplus(float,float)])], unit),
    assertion(T == float),
    gvar(new_func3, [int,int,int]).


%Testing If Statements

% If statement test1, condition, true branch, false branch
% this test passes because both branches have the same return type
test(infer_ifstat1, [nondet]) :-
    infer([if(X > float, [iplus(Q,Y)], [fToInt(Z)])], T),
    assertion(T == int),
    assertion(Q == int),
    assertion(Y == int),
    assertion(Z == float),
    assertion(X == float).

% If statement test2, condition, true branch, false branch
% this test should fail because true branch results in float, false branch results in int
test(infer_ifstat2, [fail]) :-
    infer([if(op_and(bool,bool), [fmult(Q,Y)], [idiv(A,B)])], T),
    assertion(Q == float),
    assertion(Y == float),
    assertion(A == int),
    assertion(B == int),
    assertion(T == float).


% For statement tests

% For statement test1, condition, code block
% this test should pass because last expression is an int therefore T is an int
test(infer_forstat1, [nondet]) :-
    infer([for(op_or(bool,bool), [idiv(int,X), iplus(int,Y), imult(int,Z)])], T),
    assertion(T == int),
    assertion(X == int),
    assertion(Y == int),
    assertion(Z == int).

% For statement test2, condition, code block
% this test should fail because fplus and fmult take floats and no ints
test(infer_forstat2, [fail]) :-
    infer([for(X < float, [fplus(float,int), fmult(float,int)])], T),
    assertion(T == float),
    assertion(X == float).


% Let in tests

% letin test 1-  we let v = float * float in 
%       if v > float true-branch false-branch
%       only the type of v matters because we do not care about actual values
%       this is why the if statement compares T (the type of v) with float
test(infer_letin1, [nondet]) :-
    infer([
        in(v, T, fmult(X, Y)),
        if(T > float, [fplus(A,B)], [iToFloat(Z)])
        ], T),
    assertion(T==float), 
    assertion(X==float), assertion(Y==float),
    assertion(A==float), assertion(B==float),
    assertion(Z == int),
    gvar(v,float).

% letin test 2-  we let v = float/float in 
%        for float < v print float
%        T2 should be a of type unit because the last expression in
%        the for loop is a print which returns unit
test(infer_letin2, [nondet]) :-
    infer([
        in(v, T1, fdiv(X, Y)),
        for(float < T1, [print(float)])
        ], T2),
        assertion(T1 == float),  assertion(X == float),  assertion(Y == float),
        assertion(T2 == unit).


% letin test 3-  we let v = int/int  in
%                if (boolean conditions) then let q = float plus float in 
%                                         else let q = float sub float  in
%                if (q > float)  then print float else print string
%        T3 should be type unit because final if branches resolve to unit

test(infer_letin3, [nondet]) :-
    infer([
        in(v, T1, idiv(int,int)),
        if(op_and(bool,bool), [in(q, T2, fplus(A,B))], [in(q, T2, fsub(A,B))]),
        if(T2 > float, [print(float)], [print(string)])
        ], T3),
        assertion(T1 == int),
        assertion(T2 == float), assertion(A == float), assertion(B == float), 
        assertion(T3 == unit).

% letin test 4-  we let v = op_or(X,Y) in
%                if v then int*int else int/int
% test should fail because final return type needs to be int
% but T1 gets unified with bools

test(infer_letin4, [fail]) :-
    infer([
        in(v, T1, op_or(X,Y)),
        if(T1, [imult(int,int)], [idiv(int,int)])
         ], T1),
        assertion(X == bool), assertion(Y == bool),
        assertion(T1 == bool).


% General Type Inference Tests

% Inference Test 1
% This test shows we can add the result of int multiplication
% and int divison, no types are passed yet all are infered correctly
test(infer_test1) :- 
    typeExp(iplus(imult(X,Y),idiv(A,B)), T),
    assertion(X == int), assertion(Y == int),
    assertion(A == int), assertion(B == int),
    assertion(T == int).

% Inference Test 2
% This test shows we can use nested boolean statement
% it infers the types of two OR statements within an AND statement
test(infer_test2) :- 
    typeExp(op_and(op_or(X,Y),op_or(A,B)), T),
    assertion(X == bool), assertion(Y == bool),
    assertion(A == bool), assertion(B == bool),
    assertion(T == bool).

% Inference Test 3 and 4
% These test use nearly the same code as before, but fail
% because the return type does match the type of the functions
test(infer_test3, [fail]) :- 
    typeExp(iplus(imult(X,Y),idiv(A,B)), bool),
    assertion(X == int), assertion(Y == int),
    assertion(A == int), assertion(B == int).

test(infer_test4, [fail]) :- 
    typeExp(op_and(op_or(X,Y),op_or(A,B)), int),
    assertion(X == bool), assertion(Y == bool),
    assertion(A == bool), assertion(B == bool).



:-end_tests(typeInf).

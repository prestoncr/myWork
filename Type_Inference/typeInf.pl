/* match functions by unifying with arguments 
    and infering the result
*/
% Work Completed by Christopher R Preston and Joseph Bozarth
typeExp(Fct, T):-
    \+ var(Fct), /* make sure Fct is not a variable */ 
    \+ atom(Fct), /* or an atom */
    functor(Fct, Fname, _Nargs), /* ensure we have a functor */
    !, /* if we make it here we do not try anything else */
    Fct =.. [Fname|Args], /* get list of arguments */
    append(Args, [T], FType), /* make it loook like a function signature */
    functionType(Fname, TArgs), /* get type of arguments from definition */
    typeExpList(FType, TArgs). /* recurisvely match types */

/* propagate types */
typeExp(T, T).

/* list version to allow function mathine */
typeExpList([], []).
typeExpList([Hin|Tin], [Hout|Tout]):-
    typeExp(Hin, Hout), /* type infer the head */
    typeExpList(Tin, Tout). /* recurse */

/* TODO: add statements types and their type checking */
/* global variable definition
    Example:
        gvLet(v, T, int) ~ let v = 3;
 */
typeStatement(gvLet(Name, T, Code), unit):-
    atom(Name), /* make sure we have a bound name */
    typeExp(Code, T), /* infer the type of Code and ensure it is T */
    bType(T), /* make sure we have an infered type */
    asserta(gvar(Name, T)). /* add definition to database */

/* global function definition
    Example:
        defFunc(add, args, T, int) ~ let add x y = x + y;
 */

typeStatement(defFunc(Name,Args, T, Code), unit):-
    atom(Name),
    last(Code, X),
    typeExp(X, T),
    bType(T),
    append(Args, [T], Z),
    asserta(gvar(Name,Z)). 

/* if statements
    Example:
        if(X > Y, TrueBranch, FalseBranch) ~ if x > y then ____ else ____
 */
typeStatement(if(Condition, TCode, FCode), T) :- 
    typeExp(Condition, bool),
    typeCode(TCode, T),
    typeCode(FCode, T),
    bType(T).

/* for statements
    Example:
        for(x < 10, Code) ~ for x < 10 do print x; x++ done;
 */
typeStatement(for(Condition, Code), T) :- 
    typeExp(Condition, bool),
    typeCode(Code, T),
    bType(T).

 /* let in statement
    Example
                    ~  let x = 3 in 
                            if (cond) then let y = x + x in 
                                    if (cond) then let z = y + y
                                    else then let z = y + x
                            else then let y = 0
 */   
typeStatement(in(Name, T, Code), unit):-
    atom(Name),
    typeExp(Code, T), 
    bType(T), 
    asserta(gvar(Name, T)). 

/*Code blocks are statements*/
typeStatement(Code,T) :-
    typeCode(Code, T),
    bType(T).

/*Expressions are statements*/
typeStatement(Expr,T) :-
     typeExp(Expr, T),
     bType(T).

/* Code is simply a list of statements. The type is 
    the type of the last statement 
*/
typeCode([],T) :- bType(T).
typeCode([S], T):-typeStatement(S, T).
typeCode([S1, S2], T):-
    typeStatement(S1, _T),
    typeCode(S2, T).
typeCode([S, S2|Code], T):-
    typeStatement(S,_T),
    typeCode([S2|Code], T).

/* top level function */
infer(Code, T) :-
    is_list(Code), /* make sure Code is a list */
    deleteGVars(), /* delete all global definitions */
    typeCode(Code, T).

/* Basic types
    TODO: add more types if needed
 */
bType(int).
bType(float).
bType(string).
bType(char).
bType(bool).
bType(unit). /* unit type for things that are not expressions */
/*  functions type.
    The type is a list, the last element is the return type
    E.g. add: int->int->int is represented as [int, int, int]
    and can be called as add(1,2)->3
 */
bType([H]):- bType(H).
bType([H|T]):- bType(H), bType(T).

/* Alpha Types*/
bType(T):-
     var(T).


deleteGVars():-retractall(gvar), asserta(gvar(_X,_Y):-false()).

/*  builtin functions
    Each definition specifies the name and the 
    type as a function type

    TODO: add more functions
*/

fType(iplus, [int,int,int]).
fType(fplus, [float, float, float]).
fType(isub, [int,int,int]).
fType(fsub, [float, float, float]).
fType(imult, [int,int,int]).
fType(fmult, [float, float, float]).
fType(idiv, [int,int,int]).
fType(fdiv, [float, float, float]).
fType(fToInt, [float,int]).
fType(iToFloat, [int,float]).
fType(op_and, [bool,bool,bool]).
fType(op_or, [bool,bool,bool]).
fType(print, [_X, unit]). /* simple print */
fType(identity, [T,T]).
fType('<', [float, float, bool]).
fType('>', [float, float, bool]).
fType('<=', [float, float, bool]).
fType('>=', [float, float, bool]).
fType('==', [float, float, bool]).

%fType('&&', [float, float, bool]).
%fType('||', [float, float, bool]).
%fType('!=', [float, float, bool]).


/* Find function signature
   A function is either buld in using fType or
   added as a user definition with gvar(fct, List)
*/

% Check the user defined functions first
functionType(Name, Args):-
    gvar(Name, Args),
    is_list(Args). % make sure we have a function not a simple variable

% Check first built in functions
functionType(Name, Args) :-
    fType(Name, Args), !. % make deterministic

% This gets wiped out but we have it here to make the linter happy
:- dynamic(gvar/2).


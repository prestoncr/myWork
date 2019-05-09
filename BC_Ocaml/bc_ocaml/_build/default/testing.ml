(* type env = Scope of (string, float) Hashtbl.t
  | Funcs of (string, other) Hashtlb.t
  ;;
 Hasher(Hashtbl.create 100);;
 Hasher(Hashtbl.create 100);;
================================
let my_hash = Hashtbl.create 44;;
Hashtbl.add my_hash "x" 3.3;;
my_hash;;
let q = Hashtbl.find my_hash "x";;
print_float q;;
print_newline ();;
 *)
(* type my_type = Tbl of (string, int) Hashtbl.t
;;
let a = Tbl (Hashtbl.create 100);;
let add (Tbl t) k v = Hashtbl.add t k v;;
add a "x" 3;;
let lookup (Tbl t) k = Hashtbl.find t k;;
let zz = lookup a "x";;
print_int zz;;
print_newline ();;
let my_tbl = Hashtbl.create (100);;
Hashtbl.add my_tbl "x" 3.3;; *)


(* type env =  
  Scope of (string, float) Hashtbl.t
  ;;

type envQueue = env list
;;

let addVar (str : string) (num: float) (_q: envQueue) =
  match (List.hd _q) with
  | Scope (z) -> Hashtbl.add z str num
;;

let evalExpr (_e: string) (_q:envQueue): float  = 
  match (List.hd _q) with
  | Scope (z) -> Hashtbl.find z _e
 ;;

let evalCode (_q:envQueue)  = 
  Scope (Hashtbl.create 100) :: _q |>
  addVar "x" 3.3;
  _q
;;

let zz = 
let print_me = evalExpr "x" zz;;
print_float print_me;; *)


(* type sExpr = 
  | Atom of string
  | List of sExpr list

type expr = 
  | Num of float
  | Var of string
  | Op1 of string*expr
  | Op2 of string*expr*expr
  | Fct of string * expr list

type statement = 
  | Assign of string*expr
  | Return of expr
  | Expr of expr
  | If of expr * statement list * statement list
  | While of expr*statement list
  | For of statement*expr*statement*statement list
  | FctDef of string * string list * statement list 

type block = statement list 

type env =  
Scope of (string, float) Hashtbl.t
;;
(* I think he meant -> to be completed *)
(* this "type env" needs to be a hashmap or any map really
for sure needs to map strings -> floats so they can be looked up
I imagine it will have potentially map strings to other things
as well like storing a user defined function
thought -> map strings to a custom type I will define
for now just make it map strings to floats for testing purposes *)

type envQueue = env list
(* this does not need to be changed its just the stack of environments *)

let varEval (_v: string) (_q:envQueue): float  = 0.0  

let evalExpr (_e: expr) (_q:envQueue): float  = 
  match _e with
  | Num(x) -> x
  | Var(x) -> (
      match (List.hd _q) with
      | Scope (z) -> Hashtbl.find z x
  )
  | Op1(x, y) ->  0.0
  | Op2(x, y, z) -> 0.0
  | Fct(x, y) -> 0.0 *)


(* 
  Var(stry) -> performOp2 x (Num(findVar stry _q)) z *)
let convertbool (z : float) : bool = 
  if z = 0.0 then false
  else true
;;
  let x = 3.3;;
  let y = 0.0;;
(* 
  let a = convertbool x;;
  let b = convertbool y;;

  if false||true then print_float x else print_float y;; *)

  let qq = 17.3 >= 18.5;;

  let listy = x :: y :: [];;

  print_float (List.hd listy);
print_newline ();;


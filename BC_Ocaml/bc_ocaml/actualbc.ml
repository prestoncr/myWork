type sExpr = 
  | Atom of string
  | List of sExpr list

type expr = 
  | Num of float
  | Var of string
  | Op1 of string*expr
  | Op2 of string*expr*expr
  | FctCall of string * expr list

type statement = 
  | Assign of string*expr
  | Return of expr
  | Print of expr
  | Expr of expr
  | If of expr * statement list * statement list
  | While of expr * statement list
  | For of statement * expr * statement * statement list
  | FctDef of string * string list * statement list 

type block = statement list 

type env =  
| Hasher of (string, float) Hashtbl.t
| FctStore of (string, block) Hashtbl.t
| FctParams of (string, string list) Hashtbl.t

type envQueue = env list

let convertbool (z : float) : bool = 
  if z = 0.0 then false else true

let addVar (x : string) (y : float) (z: envQueue) = 
  match z with
  | hd :: tl -> (match hd with | Hasher(q) -> Hashtbl.replace q x y) 
  | _ -> ()
 
let findVar (x : string) (z: envQueue) =
  match z with
  | hd :: tl -> (match hd with | Hasher(q) -> Hashtbl.find q x ) 
  | _ -> 0.

let addFunc (x : string) (y : block) (z: envQueue) = 
   match List.nth z ((List.length z) - 2) with
  | FctStore(q) -> Hashtbl.replace q x y
  | _ -> ()
 
let findFunc (x : string) (z: envQueue) =
  match List.nth z ((List.length z) - 2)  with
 | FctStore(q) -> Hashtbl.find q x 
  | _ -> []

let addParams (x : string) (y : string list) (z: envQueue) = 
  match List.nth z ((List.length z) - 1)  with
  | FctParams(q) -> Hashtbl.replace q x y 
  | _ -> ()

let findParams (x : string) (z: envQueue) =
  match List.nth z ((List.length z) - 1) with
  | FctParams(q) -> Hashtbl.find q x 
  | _ -> []

let indexParam (_strl : string list) (_index: int) = 
  List.nth _strl _index

let rec performOp1 (x : string) (y : expr) (q : envQueue): float = 
  match x with
  | "-"  -> 0. -. (evalExpr y q)
  | "++" -> (match y with Var(z) -> addVar z ((evalExpr y q) +. 1.) q; (evalExpr y q) +. 1. )
  | "--" -> (match y with Var(z) ->  addVar z ((evalExpr y q) -. 1.) q; (evalExpr y q) -. 1. )
  | "s" -> sin (evalExpr y q)
  | "c" -> cos (evalExpr y q)
  | "!" -> if(not(convertbool (evalExpr y q))) then 1. else 0.
  | "l" -> log (evalExpr y q)
  | "e" -> exp (evalExpr y q)
  | "sqrt" -> sqrt (evalExpr y q)
  | _ -> 0.0

 and 

performOp2 (x : string) (y : expr) (z : expr) (q: envQueue): float = 
  match x with
  | "*" -> (evalExpr y q) *. (evalExpr z q)
  | "/" -> (evalExpr y q) /. (evalExpr z q)
  | "+" -> (evalExpr y q) +. (evalExpr z q)
  | "-" -> (evalExpr y q) -. (evalExpr z q)
  | "^" -> (evalExpr y q) ** (evalExpr z q)
  | "%" -> mod_float (evalExpr y q)  (evalExpr z q)
  | "&&"-> if((convertbool (evalExpr y q))&&(convertbool (evalExpr z q)))then 1. else 0.
  | "||"-> if((convertbool (evalExpr y q))||(convertbool (evalExpr z q)))then 1. else 0.
  | ">" -> if ((evalExpr y q) > (evalExpr z q)) then 1.0 else 0.0
  | ">=" -> if ((evalExpr y q) >= (evalExpr z q)) then 1.0 else 0.0
  | "<" -> if ((evalExpr y q) < (evalExpr z q)) then 1.0 else 0.0
  | "<=" -> if ((evalExpr y q) <= (evalExpr z q)) then 1.0 else 0.0
  | "==" -> if ((evalExpr y q) == (evalExpr z q)) then 1.0 else 0.0
  | "!=" -> if ((evalExpr y q) != (evalExpr z q)) then 1.0 else 0.0
  | _ -> 0.0

 and

evalExpr (_e: expr) (_q:envQueue): float  = 
    match _e with
    | Num(x) -> x
    | Var(x) -> findVar x _q
    | Op1(x, y) -> performOp1 x y _q
    | Op2(x, y, z) -> performOp2 x y z _q
    | FctCall(x, y) -> runFunc x y _q

 and

evalStatement (q:envQueue) (s: statement): envQueue =
  match s with 
  | Expr(x) -> evalExpr x q; q
  | Assign(_v, _e) -> addVar _v (evalExpr _e q) q; q
  | Print (x) -> (print_float (evalExpr x q)); q
  | If(e, codeT, codeF) -> 
          let cond = evalExpr e q in
              if(cond>0.0) then
                  evalCode codeT q 
              else
                  evalCode codeF q
          ;q
  | While(_e, _sl) -> 
            while ((evalExpr _e q) > 0.0) do
              evalCode _sl q        
            done; q
  | For(_s1, _e, _s2, _sl) -> 
            evalStatement q _s1;
            let _sl = _s2 :: _sl in
           while ((evalExpr _e q) > 0.0) do
             evalCode _sl q        
          done; q
  | FctDef(_s, _strl, _stal) -> addFunc _s _stal q; addParams _s _strl q; q
  | Return(_e) -> addVar "ret" (evalExpr _e q) q; q
  | _ -> q

 and 

evalCode (_code: block) (_q:envQueue)  = 
   List.fold_left evalStatement _q _code 

and

runFunc (_fname: string) (_elist: expr list) (_q: envQueue) : float= 
 let _q = (Hasher(Hashtbl.create 100)) :: _q in
 let i = ref 0 in
  while (!i < (List.length _elist)) do
    addVar (indexParam (findParams _fname _q) !i) (evalExpr (List.nth _elist !i) (List.tl _q)) _q;
    i := !i + 1
  done;
  addVar "ret" 0.0 _q;
  evalCode (findFunc _fname _q) _q;
  findVar "ret" _q



let runCode (_code: block) (_q: envQueue)=
let _q = (Hasher(Hashtbl.create 100) ::FctStore(Hashtbl.create 100)
        :: FctParams(Hashtbl.create 100) ::_q) in
        evalCode _code _q
;;

let fact_block: block = 
  [ 
    If (Op2(">=", Var("n"), Num(1.0)), 
    [Return(Op2("*", Var("n"), FctCall("factorial", [Op2("-", Var("n"), Num(1.0))])))],
    [Return(Num(1.0))]
    )
  ]
;;
let factorialfunc = 
  [  
    FctDef("factorial", ["n"], fact_block);
    Print(FctCall("factorial", [Num(5.0)]))
   ]
 ;;
runCode factorialfunc [];
print_newline ();;



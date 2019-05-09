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

let convertbool (z : float) : bool = 
  if z = 0.0 then false else true


let performOp1 (x : string) (y : expr) : float = 
  match x with
  | "-"  ->( match y with | Num(z) -> (0. -. z) | _ -> 0.0  )
  | "++" ->( match y with | Num(z) -> (z +. 1.) | _ -> 0.0  )
  | "--" ->( match y with | Num(z) -> (z -. 1.) | _  -> 0.0 )
  | _ -> 0.0

let performOp2 (x : string) (y : expr) (z : expr): float = 
  match x with
  | "*" -> ( match y with | Num(a) -> ( match z with | Num(b) -> (a *. b) | _ -> 0.0 ) | _ -> 0.0 )
  | "/" -> ( match y with | Num(a) -> ( match z with | Num(b) -> (a /. b) | _ -> 0.0 ) | _ -> 0.0 )
  | "+" -> ( match y with | Num(a) -> ( match z with | Num(b) -> (a +. b) | _ -> 0.0 ) | _ -> 0.0 )
  | "-" -> ( match y with | Num(a) -> ( match z with | Num(b) -> (a -. b) | _ -> 0.0 ) | _ -> 0.0 )
  | "&&"-> ( match y with | Num(a) -> ( match z with | Num(b) -> (if((convertbool a)&&(convertbool b))then 1. else 0.) | _ -> 0.0 ) | _ -> 0.0 )
  | "||"-> ( match y with | Num(a) -> ( match z with | Num(b) -> (if((convertbool a)||(convertbool b))then 1. else 0.) | _ -> 0.0 ) | _ -> 0.0 )
  | ">" -> ( match y with | Num(a) -> ( match z with | Num(b) -> (if (a > b) then 1.0 else 0.0) | _ -> 0.0 ) | _ -> 0.0 )
  | _ -> 0.0

type block = statement list 

type env =  
| Hasher of (string, float) Hashtbl.t
| FctStore of (string, block) Hashtbl.t

type envQueue = env list

let addVar (x : string) (y : float) (z: envQueue) = 
  match z with
  | hd :: tl -> (match hd with | Hasher(q) -> Hashtbl.replace q x y) 
  | _ -> ()
 
let findVar (x : string) (z: envQueue) =
  match z with
  | hd :: tl -> (match hd with | Hasher(q) -> Hashtbl.find q x ) 
  | _ -> 0.


let addFunc (x : string) (y : block) (z: envQueue) = 
  match z with
  | hd1 :: hd2 :: tl -> (match hd2 with | FctStore(q) -> Hashtbl.replace q x y) 
  | _ -> ()
 
let findFunc (x : string) (z: envQueue) =
  match z with
  | hd1 :: hd2 :: tl -> (match hd2 with | FctStore(q) -> Hashtbl.find q x ) 
  | _ -> []

let rec evalExpr (_e: expr) (_q:envQueue): float  = 
    match _e with
    | Num(x) -> x
    | Var(x) -> findVar x _q
    | Op1(x, y) -> ( match y with 
                | Var(stry) -> performOp1 x (Num(findVar stry _q))
                | Num(numy) -> performOp1 x y 
                | _ -> 0.0
    )
    | Op2(x, y, z) -> ( match y with 
                | Var(stry) -> ( match z with | Var(strz) -> performOp2 x (Num(findVar stry _q)) (Num(findVar strz _q)) | Num(numz) -> performOp2 x (Num(findVar stry _q)) z | _ -> 0.0 )
                | Num(numy) -> ( match z with | Var(strz) -> performOp2 x y (Num(findVar strz _q)) | Num(numz) -> performOp2 x y z | _ -> 0.0 )
                | _ -> 0.0
     )
    | FctCall(x, y) -> evalCode (findFunc x _q) _q; 0.0

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
  | FctDef(_s, _strl, _stal) -> addFunc _s _stal q; q
  | _ -> q

and evalCode (_code: block) (_q:envQueue)  = 
   List.fold_left evalStatement _q _code 
;;

let runCode (_code: block) (_q: envQueue)=
let _q = (Hasher(Hashtbl.create 100) ::FctStore(Hashtbl.create 100) ::_q) in
        evalCode _code _q
;;

let p1: block = 
       [ 
         Assign("v", Op2("+", Num(9.6), Num(1.0)));
         Print (Op2("-", Var("v"), Num(2.0)))
       ]
;;

let for_block = 
  [ 
    Print (Op2("+", Var("v"), Var("v")))
  ]
;;

let trial_for= 
[  
  For(Assign("v", Op2("+", Num(3.0), Num(2.0))), Op2(">", Var("v"), Num(0.0)), Assign("v", Op2("-", Var("v"), Num(1.0))),for_block)
]
;;
runCode trial_for [];;
print_newline ();;

let my_func = FctDef("my_func", [], p1);;
let trial_func =
  [  
   my_func;
   Expr(FctCall("my_func", []))
  ]
;;

runCode trial_func [];
print_newline ();;
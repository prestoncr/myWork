open Core
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
(* 
    v = 10; 
    v // display v
 *)
 let p0: block = [
    Op2("+", Num(1.0), Num(2.2))
] 

let%expect_test "p0" =
    evalCode p0 []; 
    [%expect {| 5.2 |}]

let p1: block = [
        Assign("v", Num(1.0));
        Expr(Var("v")) 
]

let%expect_test "p1" =
    evalCode p1 []; 
    [%expect {| Not implemented |}]

(*
    v = 1.0;
    if (v>10.0) then
        v = v + 1.0
    else
        for(i=2.0; i<10.0; i++) {
            v = v * i
        }
    v   // display v
*)
let p2: block = [
    Assign("v", Num(1.0));
    If(
        Op2(">", Var("v"), Num(10.0)), 
        [Assign("v", Op2("+", Var("v"), Num(1.0)))], 
        [For(
            Assign("i", Num(2.0)),
            Op2("<", Var("i"), Num(10.0)),
            Expr(Op1("++a", Var("i"))),
            [
                Assign("v", Op2("*", Var("v"), Var("i")))
            ]
        )]
    );
    Expr(Var("v"))
]

let%expect_test "p2" =
    evalCode p2 []; 
    [%expect {| Not implemented |}]

(*  Fibbonaci sequence
    define f(x) {
        if (x<1.0) then
            return (1.0)
        else
            return (f(x-1)+f(x-2))
    }

    f(3)
    f(5)
 *)
let p3: block = 
    [
        FctDef("f", ["x"], [
            If(
                Op2("<", Var("x"), Num(1.0)),
                [Return(Num(1.0))],
                [Return(Op2("+",
                    Fct("f", [Op2("-", Var("x"), Num(1.0))]),
                    Fct("f", [Op2("-", Var("x"), Num(1.0))])
                ))])
        ]);
        Expr(Fct("f", [Num(3.0)]));
        Expr(Fct("f", [Num(5.0)]));
    ]

let%expect_test "p3" =
    evalCode p3 []; 
    [%expect {| 
       Not implemented    
    |}]




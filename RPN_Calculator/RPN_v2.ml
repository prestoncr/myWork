type token = 
  | Num of float
  | Op of string
  | TError of string

let toToken (str: string) : token = 
      match str with
      | "+" | "-" | "*" | "/" | "^" -> Op(str)
      | _ -> let f=float_of_string_opt(str) in 
          match f with
              | Some(flt) -> Num(flt)
              | None -> TError(str)

type state =
  | MyStack of float list
  | SError of string 

let evalOp (s: string) (op1: float)  (op2: float) : float =
  match s with    
      | "+" -> op1+.op2
      | "-" -> op1-.op2
      | "*" -> op1*.op2
      | "/" -> op1/.op2
      | "^" -> op1**op2
      | _ -> 0.0
     

let nextState (st: state) (t: token) : state =
  match st with
  | SError(str) -> st
  | MyStack(lst) -> 
      match t with
      | Num(f) -> MyStack(f :: lst)
      | Op(s) -> (
          match lst with
              | op2::op1::tail -> MyStack( evalOp s op1 op2 :: tail)
              | _ -> SError("Invalid RPN expression, not enough args for:  " ^ s)
          )
      | TError(s) -> SError("Unknown input:  " ^ s)


let procRPN str =
  str |> 
  String.split_on_char ' ' |> 
  List.map toToken |>
  List.fold_left nextState (MyStack [])

let q = read_line();;
let x = procRPN q;;

let print_mystack (z: state) : string =
    match z with
    | SError(f) -> f
    | MyStack(q) -> 
      match q with
        | hd1 :: hd2 :: tl -> "Not enough operators"
        |  hd :: tl -> string_of_float (List.hd q)
        | _ -> "Invalid RPN"
   
;;

let f = print_mystack x;;
print_string f;;
print_newline ();;

(* Work Completed by Christopher Preston *)
type pExp =
  | Term of int*int 
  | Plus of pExp list
  | Times of pExp list

  let rec negTerm (_e: Expr.expr) : Expr.expr = 
    match _e with
    | Num(i) -> Num(-i)
    | Var(ch) -> Mul(Num(-1),Var(ch))
    | Add(_e1, _e2) -> Add(negTerm _e1, negTerm _e2)
    | Sub(_e1, _e2) -> Sub(negTerm _e1, negTerm _e2)
    | Mul(_e1, _e2) -> (
      match _e1 with
      | Num(x) -> (
        match _e2 with
        | Var(q) -> Mul(Num(-x),Var(q))
        | Pow(q,z) -> Mul(Num(-x),Pow(q,z))
        | _ -> Mul(Num(-x), negTerm _e2)
      )
      | _ -> Mul(negTerm _e1, negTerm _e2)
    )
    | Pow(_e1, i) -> (
      match _e1 with
      | Var(x) -> Mul(Num(-1),Pow(Var(x),i))
      | _ -> Mul(Num(-1),Pow((negTerm _e1),i))
    )
    | Neg(_e1) -> Pos(_e1)
    | Pos(_e1) -> Neg(_e1)

let rec from_expr (_e: Expr.expr) : pExp =
  match _e with
  | Num(i) -> Term(i,0)
  | Var(ch) -> Term(1,1)
  | Add(_e1, _e2) -> Plus[from_expr _e1; from_expr _e2] |> concatlist
  | Sub(_e1, _e2) -> Plus[from_expr _e1; from_expr (negTerm _e2)] |> concatlist
  | Mul(_e1, _e2) -> (
    match _e1 with
    | Num(x) -> ( 
            match _e2 with 
            | Var (q) -> Term(x, 1)
            | Pow(q,z) -> Term(x,z)
            | _ -> Times[Term(x, 0); from_expr _e2]
    )
    | _ -> Times[from_expr _e1; from_expr _e2] |> concatlist
  )
  | Pow(_e1, i) -> ( 
    match _e1 with
    | Var (x) -> Term(1,i)
    | _ -> Times(evalpow i _e1) |> concatlist
  )
  | Pos(_e1) -> from_expr _e1
  | Neg(_e1) -> from_expr (negTerm _e1)

and 
evalpow ( i : int) (_e1: Expr.expr) = 
let temp_list = ref [] in for j = 0 to (i-1) do temp_list := (from_expr _e1) :: !temp_list done; !temp_list

and 
concatlist (_e : pExp) : pExp = 
  match _e with 
  | Plus(pluslist) -> (
    match List.hd pluslist with
    | Plus(x) -> Plus(List.append x (List.tl pluslist))
    | _ -> Plus(pluslist)
  )
  | Times(timeslist) -> (
    match List.hd timeslist with
  | Times(x) -> Times(List.append x (List.tl timeslist))
  | _ -> Times(timeslist)
  )
  | Term(coef,pow) -> Term(coef,pow)

let rec my_max a = 
  match a with
  [] -> invalid_arg "empty list"
  | x::xs -> List.fold_left max x xs


let rec sumInts a = 
match a with
 [] -> 0
 | h::t-> h + (sumInts t)

 let rec degree (_p:pExp): int = 
  match _p with
  | Term(coef, pow) -> pow
  | Plus(pluslist) -> ( 
    List.map termToInt pluslist |>
    my_max
  )
  | Times(timeslist) -> (
    List.map termToInt timeslist |> 
    sumInts
  )

and
 termToInt (_e: pExp): int =
  match _e with
  | Term(coef, pow) -> pow
  | Plus(x) -> degree (Plus(x))
  | Times(x) -> degree (Times(x))
 
let compare (e1: pExp) (e2: pExp) : int =
  if (degree e1 > degree e2) then -1
  else if (degree e1 == degree e2) then 0
  else 1

  let listsort a = 
    List.sort compare a

let rec catl (_e : pExp) : pExp = 
  match _e with 
  | Plus(z) ->  ( let pluslist = List.rev z in
    match List.hd pluslist with
    | Plus(x) -> Plus(listsort (List.append x (List.tl pluslist))) |> catl
    | _ -> Plus(z)
  )
  | Times(timeslist) -> (
    match List.hd timeslist with
  | Times(x) -> Times(List.append x (List.tl timeslist))
  | _ -> Times(timeslist)
  )
  | Term(coef,pow) -> Term(coef,pow) 

let rec print_pExp (_e: pExp)  =
  match _e with
  | Term (coef, pow) -> (
        match pow with
        | 0 -> print_int coef
        | 1 -> print_int coef; print_char 'x'
        | _ -> if (coef != 1) then print_int coef; print_string "x^"; print_int pow
  )
  | Plus (pluslist) ->  print_string "(";
                        for i = 0 to ((List.length pluslist)-1) 
                        do print_pExp (List.nth pluslist i); 
                        if (i != ((List.length pluslist)-1)) then print_string " + "  
                        done;
                        print_string ")"
  | Times(timeslist) -> print_string "("; 
                        for i = 0 to ((List.length timeslist)-1) 
                        do print_pExp (List.nth timeslist i); 
                        if (i != ((List.length timeslist)-1)) then print_string " * "  
                        done;
                        print_string ")"


let rec mulInts a = 
match a with
 [] -> 1
 | h::t-> h * (mulInts t)

let restoflist a = 
  match a with
  | hd1::hd2::tl -> tl
  | _ -> []

let rec simplify1 (e:pExp): pExp =
  match e with
  | Term (coef,pow) -> Term(coef,pow)
  | Plus (p) ->( let pluslist = listsort p in
      match List.hd pluslist with
      | Term (x,y) -> ( if (List.length pluslist > 1) then (
                      match List.nth pluslist 1 with
                      | Term(x1,y1) -> (if (y == y1) 
                       then                             
                       if (List.length pluslist > 2) then Plus(List.append [Term(x+x1,y)] (restoflist(pluslist)))  else Term(x+x1,y)
                       else Plus[List.hd pluslist;(simplify1 (Plus(List.tl pluslist)))] 
                      )
                      | Plus (z) -> Plus[List.hd pluslist; simplify1 (Plus(z))] 
                      | Times(z) -> Plus[List.hd pluslist; simplify1 (Times(z))] 
                      )
                      else Term(x,y)
      )
      | Plus(x) ->  Plus[List.hd pluslist; simplify1(Plus(x))]         
      | Times(x) -> Plus(List.append [simplify1(Times(x))] (List.tl pluslist)) 
  ) 
  | Times(t) -> let timeslist = t in
   match List.hd timeslist with
      | Term (x,y) -> ( if (List.length timeslist > 1) then (
                      match List.nth timeslist 1 with
                      | Term(x1,y1) ->                        
                       if (List.length timeslist > 2) then Times(List.append [Term(x*x1,y+y1)] (restoflist(timeslist)))  else Term(x*x1,y+y1)
                      | Plus (z) ->(  let temp = Times[List.hd timeslist; simplify1 (Plus(z))] in 
                       match (simplify1 (Plus(z))) with
                      | Term(qq,zz) -> temp 
                      | Plus(zz) -> distribute (List.hd timeslist) (Plus(zz)) 
                      | Times(zz) -> temp
                      )
                      | Times(z) -> Times[List.hd timeslist; simplify1 (Times(z))] 
                      )
                      else Term(x,y)
      )
      | Plus(x) ->  Times[simplify1 (List.hd timeslist); Times(List.tl timeslist)]    
      | Times(x) -> Times(List.append [simplify1(Times(x))] (List.tl timeslist)) 

and distribute (_e1: pExp) (_e2: pExp) : pExp =
  match _e1 with
  | Term (coef, pow) -> 
        match _e2 with 
        | Term (x,y) -> Term(coef*x, pow+y)
        | Plus(pluslist) ->  if ((List.length pluslist > 1)) then
             Plus[(distribute (Term(coef,pow)) (List.hd pluslist));(distribute (Term(coef,pow)) (Plus(List.tl pluslist)))] 
             else
             distribute (Term(coef,pow)) (List.hd pluslist) 
        
        | Times(timeslist) -> _e1 
  | Times(x) -> _e1 
  | Plus(x) -> _e1

let pExp_length (_e: pExp) : int = 
  match _e with
  | Term(coef, pow) -> 1
  | Plus(pluslist) -> List.length pluslist
  | Times(timeslist) -> List.length timeslist

let rec compareHeads (_e1: pExp) (_e2: pExp) : bool = 
  match _e1 with
  | Term (coef, pow) -> (
            match _e2 with 
            | Term (coef2, pow2) -> if ((coef == coef2) && (pow == pow2)) then true else false
            | Plus (x) -> false
            | Times(x) -> false
  )
  | Plus (pluslist) -> (
            match _e2 with 
            | Term (coef, pow) -> false
            | Plus (x) -> compareHeads (List.hd pluslist) (List.hd x)
            | Times(x) -> false
  )
  | Times (timeslist) -> (
            match _e2 with 
            | Term (coef, pow) -> false
            | Plus (x) -> false
            | Times(x) -> compareHeads (List.hd timeslist) (List.hd x)
  )

let compareTails (_e1: pExp) (_e2: pExp) : bool = 
  match _e1 with 
  | Term (coef, pow) -> ( 
    match _e2 with 
       | Term (x,y) -> if ((coef == x) && (pow == y)) then true else false
       | Plus (x) -> false
       | Times(x) -> false
  )
  | Plus (pluslist) -> (
    match _e2 with
       | Term (x,y) -> false
       | Plus (x) -> compareHeads (List.nth pluslist ((List.length pluslist) -1)) (List.nth x ((List.length x) -1)) 
       | Times(x) -> false
  )
  | Times(timeslist) -> (
    match _e2 with
       | Term (x,y) -> false
       | Plus (x) -> false
       | Times(x) -> compareHeads (List.nth timeslist ((List.length timeslist) -1)) (List.nth x ((List.length x) -1)) 
  )

let equal_pExp (_e1: pExp) (_e2: pExp) :bool =
  if ((pExp_length _e1) != (pExp_length _e2)) then false
  else if (not(compareHeads _e1 _e2)) then false
  else compareTails _e1 _e2

let rec simplify (e:pExp): pExp =
    let rE = simplify1(e) |> catl in
      if (equal_pExp e rE) then
        rE 
      else  
        simplify(rE) 

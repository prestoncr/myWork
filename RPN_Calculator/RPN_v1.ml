let pop_perform q op = 
  match q with
  | hd1 :: hd2 :: tl ->  (op hd2 hd1) :: tl
  | hd :: tl ->  failwith "not an RPN expression, try rearranging operands"
  | [] -> failwith "not an RPN expression, try rearranging operands"


let parse_one_string (j: 'a) (sin_str: string)  : 'a = 
    if sin_str =  "+" then ( pop_perform j ( +. ) )
    else if sin_str =  "-" then ( pop_perform j ( -. ) )
    else if sin_str =  "*" then ( pop_perform j ( *. ) )
    else if sin_str =  "/" then ( pop_perform j ( /. ) )
    else if sin_str =  "^" then ( pop_perform j ( ** ) )
    else (float_of_string sin_str) :: j

let temp = read_line()

let calculate user_input = 
  let string_list = Str.(split (regexp_string " ") user_input) in
  let x = List.fold_left parse_one_string [] string_list in
  print_float (List.hd x)
;;

calculate temp;;
print_newline ();;

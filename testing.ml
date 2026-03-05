
type const = 
Int of int 
| Bool of bool 
| Error 
| Str of string 
| Name of string
| Unit

type command = 
Push of const 
| Pop 
| Add 
| Sub 
| Mul 
| Div 
| Rem 
| Neg 
| Swap 
| ToString 
| Println 
| Quit 
| Cat 
| And 
| Or 
| Not 
| Equal 
| LessThan 
| Bind
| If
| Let
| End

type stk = const list
type env = (string * const) list
type stk_frame = {prog : command list; return_val : const; output_str : string}

let add_var (mapping  : string * const) (environment : env) : env = mapping :: environment

let rec get_var (var_name : string) (environment : env) : const option = 
  (match environment with
  | [] -> None
  | head :: tail -> 
  if(String.equal var_name (fst head)) then Some (snd head)
  else get_var var_name tail)

let rec convert_vars (num_vars : int) (stack : const list) (environment: env) : const list = 
  if(num_vars <= 0) then stack
  else
  (match stack with
  | [] -> []
  | head :: tail -> 
    (match head with
    | Name n ->
      Printf.printf ("looking for %s \n") n; 
      let value = get_var n environment in 
      (match value with
       | None -> head :: convert_vars (num_vars-1) tail environment
       | Some v -> v :: convert_vars (num_vars-1) tail environment)
    | _ -> Printf.printf ("not a name \n"); head :: convert_vars (num_vars-1) tail environment))

let dereference_n_vars (n : int) (stack : stk) (environment: env) : const list =
  let converted = convert_vars n stack environment in
  Printf.printf("converted list has %i items \n") (List.length converted);
  converted
 (*-----------------V--------------Function implementations below-----------------V--------------------*)   

let push (stack : stk) (value : const) : const list = (value :: stack)
let push_error (stack : const list) = push stack Error
let bind (stack : stk) (environment : env) : (stk * env) = 
  match stack with
  |[] -> (push_error stack, environment)
  | value :: name :: tail -> (
    match name with 
    | Name n -> 
      (match value with
      | Int _ ->  (push tail Unit, add_var (n, value) environment)
      | Str _ ->  (push tail Unit, add_var (n, value) environment)
      | Bool _ -> (push tail Unit, add_var (n, value) environment)
      | Unit ->   (push tail Unit, add_var (n, value) environment)
      | Name var -> 
        (match get_var var environment with
        | None -> (push_error stack, environment)
        | Some v -> (push tail Unit, add_var (n, v) environment))
      | _ ->      (push_error stack, environment) ) 
    | _ -> (push_error stack, environment))
  | _ -> (push_error stack, environment)

let add (stack : const list) (environment: env) : const list = 
  let lst = dereference_n_vars 2 stack environment in
  (match lst with
  |c1 :: c2 :: tail -> Printf.printf("has 2 variables");
    (match (c1, c2) with
    |(Int v1, Int v2) -> push tail (Int (v2 + v1))
    | _ -> Printf.printf("ERROR: did not convert to int"); [] )
  | _ -> Printf.printf("ERROR: dereferenced list has only %i items \n") (List.length lst); [])

  let call_func = 
    let stack = push [] (Name "a") in
    let stack = push stack (Int 1) in
    let env = [] in
    let (stack, env) = bind stack env in
    let stack = convert_vars 1 stack env in
    let stack = push stack (Name "b") in
    let stack = push stack (Name "a") in
    let (stack, env) = bind stack env in
    let stack = push stack (Name "b") in
    let stack = push stack (Name "a") in
    add stack env
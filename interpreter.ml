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
type stk_frame = {prog : command list; return_stk : stk; output_str : string}

let gimme_int (number : string) : int =
  if number = "-0" then 0 else int_of_string number

let val_is_name (value : string) : bool = 
  (* we only need to check the first char because inputs are well-formatted *)
  (String.length value >= 1) &&
  (value.[0] = '_' || 
  (value.[0] >= 'a' && value.[0] <= 'z') || 
  (value.[0] >= 'A' && value.[0] <= 'Z'))

let val_is_int (value : string) : bool = 
  match int_of_string_opt value with 
  | None -> false
  | Some _ -> true

let val_is_string (value : string) : bool = 
  let n = String.length value in
  (n >= 2 && value.[0] = '"' && value.[n - 1] = '"')

let parse_value (value : string) : const = 
  match value with 
  | ":true:"  -> Bool true
  | ":false:" -> Bool false
  | ":error:" -> Error
  | ":unit:"  -> Unit
  | _ -> if (val_is_string value) then Str (String.sub value 1 (String.length value - 2))
        else if (val_is_int value) then Int (gimme_int value)
        else if (val_is_name value) then Name value
        else Error

let parse_line(input_line : string) : command = 
  let input_line = String.trim input_line in
  match String.index_opt input_line ' ' with
   | None -> ((*no space char means it's just a regular command *)
      match input_line with
      | "pop" -> Pop
      | "add" -> Add
      | "sub" -> Sub
      | "mul" -> Mul
      | "div" -> Div
      | "rem" -> Rem
      | "neg" -> Neg
      | "swap" -> Swap
      | "toString" -> ToString
      | "println" -> Println
      | "quit" -> Quit
      | "cat" -> Cat
      | "and" -> And 
      | "or" -> Or
      | "not" -> Not
      | "equal" -> Equal
      | "lessThan" -> LessThan
      | "bind" -> Bind
      | "if" -> If
      | "let" -> Let
      | "end" -> End
        | _ -> failwith ("boy what the hell boy: " ^ input_line)
      )
    | Some index -> (* space char present means it must be a push command *)
      let cmd = String.sub input_line 0 index in
      let value = String.trim (String.sub input_line (index + 1) (String.length input_line - (index + 1)) ) in
      if (cmd = "push") then Push (parse_value value)
      else failwith ("boy what the hell boy: " ^ input_line)

let read_file (filename : string) : string list =
  (let ic = open_in filename in
  let rec loop_read acc = 
    (try 
      let l = String.trim(input_line ic) in loop_read (l::acc)
    with
      | End_of_file -> List.rev acc)
  in loop_read [] )

let parse_everything (filename : string) : command list = 
   read_file filename
  |> List.map String.trim (*remove extra white space*)
  |> List.filter (fun line -> line <> "") (*filter out empty lines*)
  |> List.map parse_line
(* ocaml is so cool *)


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
       | None -> Error :: convert_vars (num_vars-1) tail environment
       | Some v -> v :: convert_vars (num_vars-1) tail environment)
    | _ -> Printf.printf ("not a name \n"); head :: convert_vars (num_vars-1) tail environment))

let rec pull_n_items (n : int) (stack : stk) : const list = (
  if(n = 0) then [] 
  else 
    match stack with 
    | [] -> []
    | head :: tail -> head :: pull_n_items (n-1) tail)

let dereference_n_vars (n : int) (stack : stk) (environment: env) : const list =
  let converted = convert_vars n stack environment in
  Printf.printf("converted list has %i items \n") (List.length converted);
  converted
 (*-----------------V--------------Function implementations below-----------------V--------------------*)   

let push (stack : stk) (value : const) : const list = (value :: stack)

let push_error (stack : const list) = push stack Error

let peek (stack : const list) : const option= 
  (match stack with
  |[] -> None (*failwith ("attempted peek on empty stack") *)
  |head :: tail -> Some head)

 let pop (stack : const list) : const list = 
  (match stack with
  |[] -> push_error stack
  |head :: tail -> tail)

let add (stack : const list) (environment: env) : const list = 
  let lst = dereference_n_vars 2 stack environment in
  (match lst with
  |c1 :: c2 :: tail -> Printf.printf("has 2 variables");
    (match (c1, c2) with
    |(Int v1, Int v2) -> push tail (Int (v2 + v1))
    | _ -> Printf.printf("ERROR: did not convert to int"); push_error stack )
  | _ -> Printf.printf("ERROR: dereferenced list has only %i items \n") (List.length lst); push_error stack)

let sub (stack : const list) (environment: env) : const list = 
  (match dereference_n_vars 2 stack environment with
  |c1 :: c2 ::tail -> 
    (match (c1, c2) with 
    |(Int v1, Int v2) -> push tail (Int (v2 - v1))
    | _ -> push_error stack)
  | _ -> push_error stack)  

let mul (stack : const list) (environment: env) : const list = 
  (match dereference_n_vars 2 stack environment with
  |c1 :: c2 ::tail -> 
    (match (c1, c2) with
    |(Int v1, Int v2) -> push tail (Int (v1 * v2))
    | _ -> push_error stack)
  | _ -> push_error stack)  

let div (stack : const list) (environment: env) : const list = 
  (match dereference_n_vars 2 stack environment with
  |c1 :: c2 ::tail -> 
    (match (c1, c2) with
    |(Int v1, Int v2) -> 
      if (v1 = 0) then push_error stack
      else push tail (Int (v2 / v1))
    | _ -> push_error stack)
  | _ -> push_error stack)  

let rem (stack : const list) (environment: env) : const list = 
  (match dereference_n_vars 2 stack environment with
  |c1 :: c2 ::tail -> 
    (match (c1, c2) with
    |(Int v1, Int v2) -> 
      if (v1 = 0) then push_error stack
      else push tail (Int (v2 mod v1))
    | _ -> push_error stack)
  | _ -> push_error stack)  

let neg (stack : const list) (environment: env) : const list =
  (match dereference_n_vars 1 stack environment with
  |head::tail -> 
    (match head with
    |Int v -> push tail (Int (v * -1))
    | _ -> push_error stack)
  | _ -> push_error stack)

let swap (stack : const list) (environment: env) : const list = 
  (match stack with
  |c1 :: c2 ::tail -> c2 :: c1 :: tail
  | _ -> push_error stack)  

let toString (stack : const list) (environment: env) : const list =
  match stack with
  | [] -> Printf.printf("ERROR: pulled list has 0 items \n"); push_error stack
  | head :: tail ->
    (match head with 
    | Int v -> push tail (Str (string_of_int v))
    | Bool v -> 
      if (v) then push tail (Str ":true:")
      else push tail (Str ":false:")
    | Error -> push tail (Str ":error:")
    | Unit -> push tail (Str ":unit:")
    | Str v | Name v -> push tail (Str v) )

(*TODO: implement these below *)
let cat (stack : stk) (environment: env) : stk = (
  match dereference_n_vars 2 stack environment with
  |[] -> push_error stack
  | c2 :: c1 :: tail -> 
    (match c2, c1 with
    | Str v2, Str v1 -> push tail (Str (v1 ^ v2))
    | _ -> push_error stack)
  | _ -> stack)

let logic_and (stack : stk) (environment: env) : stk = (
  match dereference_n_vars 2 stack environment with
  |[] -> push_error stack
  | c2 :: c1 :: tail -> 
    (match c2, c1 with
    | Bool v2, Bool v1 -> push tail (Bool (v1 && v2))
    | _ -> push_error stack)
  | _ -> push_error stack)

let logic_or (stack : stk) (environment: env) : stk = (
  let lst = dereference_n_vars 2 stack environment in
  match lst with
  |[] -> Printf.printf("ERROR: empty\n"); push_error stack
  | c2 :: c1 :: tail -> 
    (match c2, c1 with
    | Bool v2, Bool v1 -> push tail (Bool (v1 || v2))
    | _ -> Printf.printf("ERROR: list contains non-bools\n"); push_error stack)
  | _ -> Printf.printf("ERROR: pulled list has %i items \n") (List.length lst);push_error stack)

let logic_not (stack : stk) (environment: env) : stk = (
  match dereference_n_vars 1 stack environment with
  |[] -> push_error stack
  | head :: tail -> 
    (match head with
    | Bool v -> push tail (Bool (not v))
    | _ -> push_error stack))

let logic_equal (stack : stk) (environment: env) : stk = (
  match dereference_n_vars 2 stack environment with
  |[] -> push_error stack
  | c2 :: c1 :: tail -> 
    (match c2, c1 with
    | Int v2, Int v1 -> push tail (Bool (v1 = v2))
    | _ -> push_error stack)
  | _ -> push_error stack)

let lessThan (stack : stk) (environment: env) : stk = (
  match dereference_n_vars 2 stack environment with
  |[] -> push_error stack
  | c2 :: c1 :: tail -> 
    (match c2, c1 with
    | Int v2, Int v1 -> push tail (Bool (v1 < v2))
    | _ -> push_error stack)
  | _ -> push_error stack)

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

let if_fun (stack : stk) (environment: env) : stk = (
  match dereference_n_vars 3 stack environment with
    | [] -> push_error stack
    | c1 :: c2 :: cond :: tail -> (
      match cond with
      | Bool b -> if(b) then push tail c1 else push tail c2
      | _ -> push_error stack)
    | _ -> push_error stack)


let rec run_commands (stack : stk) (environment : env) (commands : command list) (output : string) : stk_frame = 
  match commands with
  | [] -> {prog = commands; return_stk = stack; output_str = output}
  | cmd :: next -> 
    match cmd with
    | Push value  -> let new_stack = (push stack value)              in run_commands new_stack environment next output
    | Pop         -> let new_stack = (pop stack)                     in run_commands new_stack environment next output
    | Add         -> let new_stack = (add stack environment)         in run_commands new_stack environment next output
    | Sub         -> let new_stack = (sub stack environment)         in run_commands new_stack environment next output
    | Mul         -> let new_stack = (mul stack environment)         in run_commands new_stack environment next output
    | Div         -> let new_stack = (div stack environment)         in run_commands new_stack environment next output
    | Rem         -> let new_stack = (rem stack environment)         in run_commands new_stack environment next output
    | Neg         -> let new_stack = (neg stack environment)         in run_commands new_stack environment next output
    | Swap        -> let new_stack = (swap stack environment)        in run_commands new_stack environment next output
    | ToString    -> let new_stack = (toString stack environment)    in run_commands new_stack environment next output
    (*TODO : impement pattern cases below*)
    | Cat         -> let new_stack = (cat stack environment)         in run_commands new_stack environment next output
    | And         -> let new_stack = (logic_and stack environment)   in run_commands new_stack environment next output
    | Or          -> let new_stack = (logic_or stack environment)    in run_commands new_stack environment next output
    | Not         -> let new_stack = (logic_not stack environment)   in run_commands new_stack environment next output
    | Equal       -> let new_stack = (logic_equal stack environment) in run_commands new_stack environment next output
    | LessThan    -> let new_stack = (lessThan stack environment)    in run_commands new_stack environment next output
    | Bind        -> let b = (bind stack environment )               in run_commands (fst b) (snd b) next output
    | If          -> let new_stack = (if_fun stack environment)      in run_commands new_stack environment next output
    | Let         ->  
      let frame = run_commands stack environment next output in 
      let new_stack =
      (match peek frame.return_stk with
      | None -> stack
      | Some v -> push stack v) in
      run_commands new_stack environment frame.prog frame.output_str
    | End         -> {prog = next; return_stk = stack; output_str = output}
    (*^^^^^^^^^^^^^^^*)
    | Println -> 
      (match stack with
      | h :: t -> 
        (match h with
        | Str s -> run_commands (pop stack) environment next (output ^ s ^ "\n") 
        | _ -> run_commands (push_error stack) environment next output)
      | _ -> run_commands (push_error stack) environment next output)
    | Quit -> {prog = commands; return_stk = stack; output_str = output}
  
    
let interpreter (input : string) (output : string) : unit = 
  let commands = parse_everything input in
  let return =  run_commands [] [] commands "" in
  let final_output = return.output_str in
  Out_channel.with_open_text output (fun channel -> Out_channel.output_string channel final_output)
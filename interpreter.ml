type const = Num of int | String of string | Boolean of bool | Error | Name of string | Unit | None
type command = Push of const | Pop | Add | Sub | Mul | Div | Rem | Neg | Swap | ToString | PrintLn | Quit | Cat | And | Or | Not 
              | Equal | LessThan | Bind | If | Let | End | Fun of const * const | InOutFun of const * const | FunEnd | Return | Call
type func = Function of const * command list | FunName of const | InOutFunction of (const) * command list | FunError 
							| IOFunOut of const * const | IOFunOutRet of const * const * const | FunOut of const


let constToString (c : const) =
  match c with
    | Num i -> "Num("^(string_of_int i)^")"
    | String s -> "String("^s^")"
    | Boolean b -> "Boolean(:"^(string_of_bool b)^":)"
    | Error -> "Error(:error:)"
    | Name s -> "Name("^s^")"
    | Unit -> "Unit(:unit:)"
		| None -> "None";;

    let rec print_bindings (bindings) =
      match bindings with
      | k::v::rest -> print_string ("{"^(constToString k)^":"^(constToString v)^"} \n"); print_bindings rest
      | [] -> ()
      | _ -> ();;

let rec find_binding (key : string) (rem_bindings) (all_bindings): const = 
  (* print_string ("Find "^key^"\n"); *)
  (* print_bindings rem_bindings; *)
  match rem_bindings with
  | [] -> Error
  | k::[] -> Error
  | k::v::other -> (
    (* print_string ("{{{{"^(constToString k)^":"^(constToString v)^"}}}}\n"); *)
    match k with
    | Name kn -> (
    (* print_string (key^" == "^kn^" : "); *)
    (* print_string ((string_of_bool (key = kn))^"\n"); *)
    if key = kn then 
      (
        (* print_string "FOUND\n"; *)
        match v with
       | Name n -> 
        (let n_binding = find_binding n all_bindings all_bindings in
        match n_binding with
        | Error -> Name n
        | _ -> n_binding)
        | _ -> 
        (* print_string ("Return " ^ (constToString v) ^ "\n"); *)
         v
      )
      else
        find_binding key other all_bindings
  )
  | _ -> Error    
);;

 let push (value : const) (stack : const list) : const list = value::stack

let pop (stack : const list) : const list =
  match stack with
  | [] -> push Error stack
  | head::tail -> tail;;

let add (stack : const list) (bindings): const list = 
    match stack with
    | [] -> push Error stack
    | any::[] -> push Error stack
    | x::y::newStack -> 
      
      (match (x, y) with
      | (Num xi, Num yi) ->  push (Num (yi + xi)) newStack
      | (Name xn, Num yi) -> (
        let bindX = find_binding xn bindings bindings in
        match bindX with
        | Num ib -> push (Num (ib + yi)) newStack
        | _ -> push Error stack
      )
      | (Num xi, Name yn) -> (
        let bindY = find_binding yn bindings bindings in
        match bindY with
        | Num ib -> push (Num (ib + xi)) newStack
        | _ -> push Error stack
      )
      | (Name xn, Name yn) -> (
        let bindX = find_binding xn bindings bindings in
        let bindY = find_binding yn bindings bindings in
        match (bindX, bindY) with
        | (Num ibx, Num iby) -> push (Num (ibx + iby)) newStack
        | _ -> push Error stack
      )
      | (any1, any2) -> push Error stack
      )

let sub (stack : const list) (bindings) = 
				match stack with
				| [] -> push Error stack
				| any::[] -> push Error stack
				| x::y::newStack -> 
					(match (x, y) with
					| (Num xi, Num yi) -> push (Num (yi - xi)) newStack
					| (Name xn, Num yi) -> (
						let bindX = find_binding xn bindings bindings in
						match bindX with
						| Num ib -> push (Num (yi - ib)) newStack
						| _ -> push Error stack
					)
					| (Num xi, Name yn) -> (
						let bindX = find_binding yn bindings bindings in
						match bindX with
						| Num ib -> push (Num (ib - xi)) newStack
						| _ -> push Error stack
					)
					| (Name xn, Name yn) -> (
						let bindX = find_binding xn bindings bindings in
						let bindY = find_binding yn bindings bindings in
						match (bindX, bindY) with
						| (Num ibx, Num iby) -> push (Num (iby - ibx)) newStack
						| _ -> push Error stack
					)
					| (any1, any2) -> push Error stack
					);;

let mul (stack : const list) (bindings) = 
	match stack with
	| [] -> push Error stack
	| any::[] -> push Error stack
	| x::y::newStack -> 
		(match (x, y) with
		| (Num xi, Num yi) -> push (Num (yi * xi)) newStack
		| (Name xn, Num yi) -> (
			let bindX = find_binding xn bindings bindings in
			match bindX with
			| Num ib -> push (Num (ib * yi)) newStack
			| _ -> push Error stack
		)
		| (Num xi, Name yn) -> (
			let bindX = find_binding yn bindings bindings in
			match bindX with
			| Num ib -> push (Num (ib * xi)) newStack
			| _ -> push Error stack
		)
		| (Name xn, Name yn) -> (
			let bindX = find_binding xn bindings bindings in
			let bindY = find_binding yn bindings bindings in
			match (bindX, bindY) with
			| (Num ibx, Num iby) -> push (Num (iby * ibx)) newStack
			| _ -> push Error stack
		)
		| (any1, any2) -> push Error stack
		);;

let div (stack : const list) (bindings)= 
(* print_string "DIV CALLED\n"; *)
match stack with
| [] -> push Error stack
| any::[] -> push Error stack
| x::y::newStack -> 
	(* print_string ((constToString x)^" "^(constToString y)^"\n");  *)
	(match (x, y) with
	| (Num 0, Num yi) -> push Error stack
	| (Num xi, Num yi) -> push (Num (yi / xi)) newStack
	| (Name xn, Num yi) -> (
		(* print_string "Div "; print_int yi; print_string ("/ " ^ xn ^ "\n\n"); *)
		let bindX = find_binding xn bindings bindings in
		match bindX with
		| Num ib -> 
			(* print_string "Div "; print_int yi; print_string " / "; print_int ib; print_string "\n\n"; *)
			push (Num (yi / ib)) newStack
		| _ -> push Error stack
	)
	| (Num xi, Name yn) -> (
		(* print_string "Div "; print_string (yn ^ " / "); print_int xi; print_string "\n\n"; *)
		let bindX = find_binding yn bindings bindings in
		match bindX with
		| Num ib -> 
			(* print_string "Div "; print_int ib; print_string (" / "); print_int xi; print_string "\n\n"; *)
			push (Num (ib / xi)) newStack
		| _ -> push Error stack
	)
	| (Name xn, Name yn) -> (
		let bindX = find_binding xn bindings bindings in
		let bindY = find_binding yn bindings bindings in
		match (bindX, bindY) with
		| (Num ibx, Num iby) -> push (Num (iby / ibx)) newStack
		| _ -> push Error stack
	)
	| (any1, any2) -> push Error stack
	);;

let rem (stack : const list) (bindings) = 
	match stack with
	| [] -> push Error stack
	| any::[] -> push Error stack
	| x::y::newStack -> 
		(match (x, y) with
		| (Num 0, Num yi) -> push Error stack
		| (Num xi, Num yi) -> push (Num (yi mod xi)) newStack
		| (Name xn, Num yi) -> (
			let bindX = find_binding xn bindings bindings in
			match bindX with
			| Num ib -> push (Num (ib mod yi)) newStack
			| _ -> push Error stack
		)
		| (Num xi, Name yn) -> (
			let bindX = find_binding yn bindings bindings in
			match bindX with
			| Num ib -> push (Num (ib mod xi)) newStack
			| _ -> push Error stack
		)
		| (Name xn, Name yn) -> (
			let bindX = find_binding xn bindings bindings in
			let bindY = find_binding yn bindings bindings in
			match (bindX, bindY) with
			| (Num ibx, Num iby) -> push (Num (iby mod ibx)) newStack
			| _ -> push Error stack
		)
		| (any1, any2) -> push Error stack
		);;

let neg (stack : const list) (bindings) = 
	match stack with 
	| [] -> push Error stack
	| x::newStack -> 
		(match x with
		| Num xi -> push (Num (xi * -1)) newStack
		| Name xn -> (
			let bindX = find_binding xn bindings bindings in
			match bindX with
			| Num xi -> push (Num (xi * -1)) newStack
			| _ -> push Error stack
			)
		| any -> push Error stack
		);;

let swap (stack : const list) =
	match stack with 
	| [] -> push Error stack
	| any::[] -> push Error stack
	| x::y::newStack -> [y] @ [x] @ newStack;;

let toString (stack : const list) =
	match stack with
	| [] -> push Error stack
	| x::newStack -> 
		(match x with
		| Num i -> push (String (string_of_int i)) newStack
		| String s -> push (String s) newStack
		| Boolean b -> push (String (":"^(string_of_bool b)^":")) newStack
		| Error -> 
			(* print_string "ERROR ON toString"; *)
			push (String ":error:") newStack
		| Name s -> push (String s) newStack
		| Unit -> push (String ":unit:") newStack
		| None -> push (String ":none:") newStack
		);;

let cat (stack : const list) (bindings) =
	match stack with
	| [] -> push Error stack
	| x::[] -> push Error stack
	| x::y::newStack ->
		(match (x, y) with
		| (String sx, String sy) -> push (String (sy ^ sx)) newStack
		| (Name nx, String sy) -> (
			let bindX = find_binding nx bindings bindings in
			match bindX with
			| String sb -> push (String (sy ^ sb)) newStack
			| _ -> push Error stack
		)
		| (String sx, Name ny) -> (
			let bindX = find_binding ny bindings bindings in
			match bindX with
			| String sb -> push (String (sb ^ sx)) newStack
			| _ -> push Error stack
		)
		| (Name nx, Name ny) -> (
			let bindX = find_binding nx bindings bindings in
			let bindY = find_binding ny bindings bindings in
			match (bindX, bindY) with
			| (String sbx, String sby) -> push (String (sbx ^ sby)) newStack
			| _ -> push Error stack
		)
		| (_, _) -> push Error stack
		);;

let andOp (stack : const list) (bindings) =
	match stack with
	| [] -> push Error stack
	| _::[] -> push Error stack
	| x::y::newStack -> 
		(match (x, y) with
		| (Boolean bx, Boolean by) -> push (Boolean (bx && by)) newStack
		| (Name xn, Boolean yb) -> (
			let bindX = find_binding xn bindings bindings in
			match bindX with 
			| Boolean xb -> push (Boolean (xb && yb)) newStack
			| _ -> push Error stack
		)
		| (Boolean xb, Name yn) -> (
			let bindY = find_binding yn bindings bindings in
			match bindY with 
			| Boolean yb -> push (Boolean (xb && yb)) newStack
			| _ -> push Error stack
		)
		| (Name xn, Name yn) -> (
			let bindX = find_binding xn bindings bindings in
			let bindY = find_binding yn bindings bindings in
			match (bindX, bindY) with 
			| (Boolean xb, Boolean yb) -> push (Boolean (xb && yb)) newStack
			| _ -> push Error stack
		)
		| (_, _) -> push Error stack
		);;

let orOp (stack : const list) (bindings) =
	match stack with
	| [] -> push Error stack
	| _::[] -> push Error stack
	| x::y::newStack ->
		(match (x, y) with
		| (Boolean bx, Boolean by) -> push (Boolean (bx || by)) newStack
		| (Name xn, Boolean yb) -> (
			let bindX = find_binding xn bindings bindings in
			match bindX with 
			| Boolean xb -> push (Boolean (xb || yb)) newStack
			| _ -> push Error stack
		)
		| (Boolean xb, Name yn) -> (
			let bindY = find_binding yn bindings bindings in
			match bindY with 
			| Boolean yb -> push (Boolean (xb || yb)) newStack
			| _ -> push Error stack
		)
		| (Name xn, Name yn) -> (
			let bindX = find_binding xn bindings bindings in
			let bindY = find_binding yn bindings bindings in
			match (bindX, bindY) with 
			| (Boolean xb, Boolean yb) -> push (Boolean (xb && yb)) newStack
			| _ -> push Error stack
		)
		| (_, _) -> push Error stack
		);;

let notOp (stack : const list) (bindings) =
	match stack with
	| [] -> push Error stack
	| top::newStack ->
		(match top with
		| Boolean b -> push (Boolean (not b)) newStack
		| Name n -> (
			let bindX = find_binding n bindings bindings in
			match bindX with
			| Boolean b -> push (Boolean (not b)) newStack
			| _ -> push Error stack
		)
		| _ -> push Error stack
		);;

let equalOp (stack : const list) (bindings) =
	match stack with
	| [] -> push Error stack
	| _::[] -> push Error stack
	| x::y::newStack ->
		(match (x, y) with
		| (Num xi, Num yi) -> push (Boolean (xi == yi)) newStack
		| (Name xn, Num yi) -> (
			let bindX = find_binding xn bindings bindings in
			match bindX with 
			| Num xi -> push (Boolean (xi == yi)) newStack
			| _ -> push Error stack
		)
		| (Num xi, Name yn) -> (
			let bindY = find_binding yn bindings bindings in
			match bindY with 
			| Num yi -> push (Boolean (xi == yi)) newStack
			| _ -> push Error stack
		)
		| (Name xn, Name yn) -> (
			let bindX = find_binding xn bindings bindings in
			let bindY = find_binding yn bindings bindings in
			match (bindX, bindY) with 
			| (Num xi, Num yi) -> push (Boolean (xi == yi)) newStack
			| _ -> push Error stack
		)
		| (_, _) -> push Error stack
		);;

let lessThan (stack : const list) (bindings) =
	match stack with
	| [] -> push Error stack
	| _::[] -> push Error stack
	| x::y::newStack ->
		(match (x, y) with
		| (Num xi, Num yi) -> push (Boolean (y < x)) newStack
		| (Name xn, Num yi) -> (
			let bindX = find_binding xn bindings bindings in
			match bindX with 
			| Num xi -> push (Boolean (yi < xi)) newStack
			| _ -> push Error stack
		)
		| (Num xi, Name yn) -> (
			let bindY = find_binding yn bindings bindings in
			match bindY with 
			| Num yi -> push (Boolean (yi < xi)) newStack
			| _ -> push Error stack
		)
		| (Name xn, Name yn) -> (
			let bindX = find_binding xn bindings bindings in
			let bindY = find_binding yn bindings bindings in
			match (bindX, bindY) with 
			| (Num xi, Num yi) -> push (Boolean (yi < xi)) newStack
			| _ -> push Error stack
		)
		| (_, _) -> push Error stack
		);;

let bind (stack : const list) (bindings)=
match stack with
| [] -> push Error stack
| _::[] -> push Error stack
| x::y::newStack ->
	(match (x, y) with
	| (Num i, Name n) -> push Unit newStack
	| (String s, Name n) -> push Unit newStack
	| (Boolean b, Name n)  -> push Unit newStack
	| (Unit, Name n) -> push Unit newStack
	| (Name n1, Name n2) -> (
		let nameBind = find_binding n2 bindings bindings in
		if nameBind = Error then
			push Error stack
		else push Unit newStack)
	| (_, _) -> push Error stack
	);;

let rec add_binding (stack : const list) (prev_bindings) (rem_bindings) =
	match stack with
	| [] -> push Error stack
	| _::[] -> push Error stack
	| x::y::newStack ->
		(
			(* print_string ("Binding {"^(constToString y)^":"^(constToString x)^"} \n");  *)
		match rem_bindings with
		| [] -> (
			match y with
			| Name n -> prev_bindings@[y]@[x]
			| _ -> prev_bindings
		)
		| k::v::rest -> (
			match (k, y) with
			| (Name kn, Name yn) -> (
				if (String.equal kn yn) then 
					prev_bindings@[y]@[x]@rest
				else
					add_binding stack (prev_bindings@[k]@[v]) rest
			)
			| _ -> add_binding stack (prev_bindings@[k]@[v]) rest
			(* print_string (constToString k); print_string " == "; print_string (constToString y); print_string "\n";
			if k = y then 
				prev_bindings@[y]@[x]@rem_bindings
			else
				add_binding stack (prev_bindings@[k]@[v]) rest *)
			)
		| _ -> prev_bindings@rem_bindings
		);;

let rec add_fun new_fun_name new_fun_obj prev_funBindings rem_funBindings =
	match rem_funBindings with
	| name::obj::tail -> (
		match (name, new_fun_name) with 
		| (FunName (Name n), FunName (Name nfn)) -> (
			let same_name = (String.equal n nfn) in
			if same_name then
				prev_funBindings @ [new_fun_name] @ [new_fun_obj] @ tail
			else
				add_fun new_fun_name new_fun_obj (prev_funBindings @ [name] @ [obj]) tail
		)
		| _ -> add_fun new_fun_name new_fun_obj (prev_funBindings @ [name] @ [obj]) tail
	)
	| [] -> prev_funBindings @ [new_fun_name] @ [new_fun_obj]
	| _ -> prev_funBindings @ [new_fun_name] @ [new_fun_obj];;

let rec replace_binding (key : const) (value : const) (prev_bindings) (rem_bindings) =
	match rem_bindings with 
	| [] -> prev_bindings
	| x::[] -> prev_bindings @ [x]
	| k::v::tail -> (
		match (k, key) with
		| (Name k_name, Name key_name) ->
			let same_key = (String.equal k_name key_name) in
			if same_key then
				prev_bindings @ [key] @ [value] @ tail
			else
				replace_binding key value (prev_bindings @ [k] @ [v]) tail
		| _ -> replace_binding key value (prev_bindings @ [k] @ [v]) tail
	);;

let ifOp (stack : const list) (bindings) = 
	match stack with
	| [] -> push Error stack
	| _::[] -> push Error stack
	| _::_::[] -> push Error stack
	| x::y::z::newStack -> 
		let z_bool = (match z with
		| Boolean bz -> Boolean bz
		| Name nz -> find_binding nz bindings bindings
		| _ -> Error
		) in
		(match (x, y, z_bool) with
		| (x_const, y_const, Boolean bz) -> if bz then push x_const newStack else push y_const newStack
		| _ -> push Error stack
		);;

let rec letCommands (commands : command list) (nests : int) =
	match commands with
	| [] -> []
	| com::tail ->
		(match com with
		| End -> if nests == 0 then tail else letCommands tail (nests - 1)
		| Let -> letCommands tail (nests + 1)
		| _ -> letCommands tail nests
		);;

let rec print_stack (stack : const list) = 
	match stack with
	| [] -> ()
	| top::bottom -> print_string ((constToString top)^"\n"); print_stack bottom;;

let commandToString (c : command) = 
	match c with
	| Push v -> "Push "^(constToString v)
	| Pop -> "Pop"
	| Add -> "Add"
	| Sub -> "Sub"
	| Mul -> "Mul"
	| Div -> "Div"
	| Rem -> "Rem"
	| Neg -> "Neg"
	| Swap -> "Swap"
	| ToString -> "ToString"
	| PrintLn -> "PrintLn"
	| Quit -> "Quit" 
	| Cat -> "Cat"
	| And -> "And"
	| Or -> "Or"
	| Not -> "Not"
	| Equal -> "Equal"
	| LessThan -> "LessThan"
	| Bind -> "Bind"
	| If -> "If"
	| Let -> "Let"
	| End -> "End"
	| Fun (Name n, Name a) -> "Fun "^n^" "^a
	| FunEnd -> "FunEnd"
	| Return -> "Return"
	| Call -> "Call"
	| InOutFun (Name n, Name a) -> "InOutFun "^n^" "^a
	| _ -> "OTHER";;

let rec getFunCommands ((funCommands : command list), (commands : command list)) = 
	match commands with
	| FunEnd::tail -> funCommands
	| com::tail -> getFunCommands ((funCommands @ [com]), tail)
	| [] -> [];;

let rec getAfterFun (commands : command list) = 
	match commands with
	| FunEnd::tail -> tail
	| com::tail -> getAfterFun tail
	| [] -> [];;

	let rec letStack2 ((commands : command list), (stack: const list), (bindings), (funBindings)) =
		match commands with
		| com::tail -> 
			print_string "-----Let Bindings-----\n";
			print_bindings bindings;
			print_string "-----Let Stack-----\n";
			print_stack stack;
			print_string "-------------------\n\n";
			print_string "-----Let Command------\n";
			print_string (commandToString com^"\n");
			(match com with
			| Push v ->  letStack2 (tail, (push v stack), bindings, funBindings)
			| Pop -> letStack2 (tail, (pop stack), bindings, funBindings)
			| Add -> letStack2 (tail, (add stack bindings), bindings, funBindings)
			| Sub -> letStack2 (tail, (sub stack bindings), bindings, funBindings)
			| Mul -> letStack2 (tail, (mul stack bindings), bindings, funBindings)
			| Div -> letStack2 (tail, (div stack bindings), bindings, funBindings)
			| Rem -> letStack2 (tail, (rem stack bindings), bindings, funBindings)
			| Neg -> letStack2 (tail, (neg stack bindings), bindings, funBindings)
			| Swap -> letStack2 (tail, (swap stack), bindings, funBindings)
			| ToString -> letStack2(tail, (toString stack), bindings, funBindings)
			| PrintLn -> letStack2(tail, stack, bindings, funBindings)
			| Quit -> Error
			| Cat -> letStack2 (tail, (cat stack bindings), bindings, funBindings)
			| And -> letStack2 (tail, (andOp stack bindings), bindings, funBindings)
			| Or -> letStack2 (tail, (orOp stack bindings), bindings, funBindings)
			| Not -> letStack2 (tail, (notOp stack bindings), bindings, funBindings)
			| Equal -> letStack2 (tail, (equalOp stack bindings), bindings, funBindings)
			| LessThan -> letStack2 (tail, (lessThan stack bindings), bindings, funBindings)
			| Bind -> letStack2 (tail, (bind stack bindings), (add_binding stack [] bindings), funBindings)
			| If -> letStack2 (tail, (ifOp stack bindings), bindings, funBindings)
			| Let -> 
				let commands_after_let = letCommands tail 0 in
				let let_result = letStack2 (tail, stack, bindings, funBindings) in
				letStack2 (commands_after_let, (push let_result stack), bindings, funBindings)
			| End -> 
				(match stack with
				| [] -> Error
				| head::tail -> head
				)
			| Fun (fun_name, fun_arg) -> 
				let fun_commands = getFunCommands ([], tail) in
				let commands_after_fun = getAfterFun tail in
				let funObj = Function (fun_arg, fun_commands) in
				letStack2 (commands_after_fun, (push Unit stack), bindings, (add_fun (FunName fun_name) funObj [] funBindings))
			| FunEnd -> letStack2 (tail, (push Error stack), bindings, funBindings)
			| Return -> letStack2 (tail, (push Error stack), bindings, funBindings)
			| Call -> letStack2 (tail, (push Error stack), bindings, funBindings)
			| InOutFun (fun_name, fun_arg) -> 
				let fun_commands = getFunCommands ([], tail) in
				let commands_after_fun = getAfterFun tail in
				let funObj = InOutFunction (fun_arg, fun_commands @ [FunEnd]) in
				letStack2 (commands_after_fun, (push Unit stack), bindings, (add_fun (FunName fun_name) funObj [] funBindings))
			)
		| _ -> Error;;

let rec find_function (fun_name : string) (funBindings) =
	match funBindings with
	| [] -> 
		print_string "Not Found\n";
		FunError
	| n::[] -> FunError
	| n::f::tail -> (
		match (n, f) with
		| (FunName (Name fn), Function (a, c)) -> (
			if fn = fun_name then
				Function (a, c)
			else
				find_function fun_name tail
		)
		| (FunName (Name fn), InOutFunction (a, c)) -> (
			if fn = fun_name then
				InOutFunction (a, c)
			else
				find_function fun_name tail
		)
		| _ -> FunError
	);;

let rec execute_function ((fun_commands), (arg), (arg_binding), (stack : const list), (fun_stack), (bindings), (funBindings)) = 
	print_string "------RegFun Bindings------\n";
	print_bindings bindings;
	print_string "------RegFun Stack------\n";
	print_stack fun_stack;
	print_string "------RegFun Command------\n";
	match fun_commands with
	| [] -> FunError
	| com::tail -> (
		print_string (commandToString com); print_string "\n";
		print_string ("--------------------\n");
		match com with
		| Push c -> execute_function (tail, arg, arg_binding, stack, (push c fun_stack), bindings, funBindings)
		| Pop -> execute_function (tail, arg, arg_binding, stack, (pop fun_stack), bindings, funBindings)
		| Add -> execute_function (tail, arg, arg_binding, stack, (add fun_stack (bindings @ [arg_binding] @ [arg])), bindings, funBindings)
		| Sub -> execute_function (tail, arg, arg_binding, stack, (sub fun_stack (bindings @ [arg_binding] @ [arg])), bindings, funBindings)
		| Mul -> execute_function (tail, arg, arg_binding, stack, (mul fun_stack (bindings @ [arg_binding] @ [arg])), bindings, funBindings)
		| Div -> execute_function (tail, arg, arg_binding, stack, (div fun_stack (bindings @ [arg_binding] @ [arg])), bindings, funBindings)
		| Rem -> execute_function (tail, arg, arg_binding, stack, (rem fun_stack (bindings @ [arg_binding] @ [arg])), bindings, funBindings)
		| Neg -> execute_function (tail, arg, arg_binding, stack, (neg fun_stack (bindings @ [arg_binding] @ [arg])), bindings, funBindings)
		| Swap -> execute_function (tail, arg, arg_binding, stack, (swap fun_stack), bindings, funBindings)
		| ToString -> execute_function (tail, arg, arg_binding, stack, (toString fun_stack), bindings, funBindings)
		| PrintLn -> execute_function (tail, arg, arg_binding, stack, fun_stack, bindings, funBindings)
		| Quit -> FunError
		| Cat -> execute_function (tail, arg, arg_binding, stack, (cat fun_stack (bindings @ [arg_binding] @ [arg])), bindings, funBindings)
		| And -> execute_function (tail, arg, arg_binding, stack, (andOp fun_stack (bindings @ [arg_binding] @ [arg])), bindings, funBindings)
		| Or -> execute_function (tail, arg, arg_binding, stack, (orOp fun_stack (bindings @ [arg_binding] @ [arg])), bindings, funBindings)
		| Not -> execute_function (tail, arg, arg_binding, stack, (notOp fun_stack (bindings @ [arg_binding] @ [arg])), bindings, funBindings)
		| Equal -> execute_function (tail, arg, arg_binding, stack, (equalOp fun_stack (bindings @ [arg_binding] @ [arg])), bindings, funBindings)
		| LessThan -> execute_function (tail, arg, arg_binding, stack, (lessThan fun_stack (bindings @ [arg_binding] @ [arg])), bindings, funBindings)
		| Bind -> execute_function (tail, arg, arg_binding, stack, (bind fun_stack bindings), (add_binding fun_stack [] bindings), funBindings)
		| If -> execute_function (tail, arg, arg_binding, stack, (ifOp fun_stack bindings), bindings, funBindings)
		| Let -> print_string "------LET------\n";
			let commands_after_end = letCommands tail 0 in
			let let_result = letStack2 (tail, stack, bindings, funBindings) in
			print_string "-----END LET------\n";
			execute_function (commands_after_end, arg, arg_binding, stack, (push let_result fun_stack), bindings, funBindings)
		| End -> FunError
		| Fun (n, a) -> (
			let fun_commands = getFunCommands ([], tail) in
			let commands_after_fun = getAfterFun tail in
			let funObj = Function (a, fun_commands) in
			execute_function (commands_after_fun, arg, arg_binding, stack, (push Unit stack), bindings, (funBindings @ [FunName n] @ [funObj]))
		)
		| Call -> FunError
		| Return -> (
			match fun_stack with
			| head::tail -> FunOut head
			| [] -> FunError
		)
		| FunEnd -> FunOut None
		| _ -> FunError
	);;

let rec execute_inout_function (fun_commands, arg, (arg_binding : const), (input_arg : const), (stack : const list), fun_stack, bindings, funBindings) =
	print_string "------InOutFun Bindings------\n";
	print_bindings bindings;
	print_string "------InOutFun Stack------\n";
	print_stack fun_stack;
	print_string "--------------------\n\n";
	match fun_commands with
	| [] -> FunError
	| com::tail -> (
		print_string "------InOutFun Command------\n";
		print_string (commandToString com); print_string "\n";
		match com with
		| Push c -> execute_inout_function (tail, arg, arg_binding, input_arg, stack, (push c fun_stack), bindings, funBindings)
		| Pop -> execute_inout_function (tail, arg, arg_binding, input_arg, stack, (pop fun_stack), bindings, funBindings)
		| Add -> execute_inout_function (tail, arg, arg_binding, input_arg, stack, (add fun_stack (bindings @ [arg_binding] @ [arg])), bindings, funBindings)
		| Sub -> execute_inout_function (tail, arg, arg_binding, input_arg, stack, (sub fun_stack (bindings @ [arg_binding] @ [arg])), bindings, funBindings)
		| Mul -> execute_inout_function (tail, arg, arg_binding, input_arg, stack, (mul fun_stack (bindings @ [arg_binding] @ [arg])), bindings, funBindings)
		| Div -> execute_inout_function (tail, arg, arg_binding, input_arg, stack, (div fun_stack (bindings @ [arg_binding] @ [arg])), bindings, funBindings)
		| Rem -> execute_inout_function (tail, arg, arg_binding, input_arg, stack, (rem fun_stack (bindings @ [arg_binding] @ [arg])), bindings, funBindings)
		| Neg -> execute_inout_function (tail, arg, arg_binding, input_arg, stack, (neg fun_stack (bindings @ [arg_binding] @ [arg])), bindings, funBindings)
		| Swap -> execute_inout_function (tail, arg, arg_binding, input_arg, stack, (swap fun_stack), bindings, funBindings)
		| ToString -> execute_inout_function (tail, arg, arg_binding, input_arg, stack, (toString fun_stack), bindings, funBindings)
		| PrintLn -> execute_inout_function (tail, arg, arg_binding, input_arg, stack, fun_stack, bindings, funBindings)
		| Quit -> FunError
		| Cat -> execute_inout_function (tail, arg, arg_binding, input_arg, stack, (cat fun_stack (bindings @ [arg_binding] @ [arg])), bindings, funBindings)
		| And -> execute_inout_function (tail, arg, arg_binding, input_arg, stack, (andOp fun_stack (bindings @ [arg_binding] @ [arg])), bindings, funBindings)
		| Or -> execute_inout_function (tail, arg, arg_binding, input_arg, stack, (orOp fun_stack (bindings @ [arg_binding] @ [arg])), bindings, funBindings)
		| Not -> execute_inout_function (tail, arg, arg_binding, input_arg, stack, (notOp fun_stack (bindings @ [arg_binding] @ [arg])), bindings, funBindings)
		| Equal -> execute_inout_function (tail, arg, arg_binding, input_arg, stack, (equalOp fun_stack (bindings @ [arg_binding] @ [arg])), bindings, funBindings)
		| LessThan -> execute_inout_function (tail, arg, arg_binding, input_arg, stack, (lessThan fun_stack (bindings @ [arg_binding] @ [arg])), bindings, funBindings)
		| Bind -> execute_inout_function (tail, arg, arg_binding, input_arg, stack, (bind fun_stack bindings), (add_binding fun_stack [] bindings), funBindings)
		| If -> execute_inout_function (tail, arg, arg_binding, input_arg, stack, (ifOp fun_stack bindings), bindings, funBindings)
		| Let -> FunError
		| End -> FunError
		| Fun (n, a) -> (
			let fun_commands = getFunCommands ([], tail) in
			let commands_after_fun = getAfterFun tail in
			let funObj = Function (a, fun_commands) in
			execute_inout_function (commands_after_fun, arg, arg_binding, input_arg, stack, (push Unit stack), bindings, (funBindings @ [FunName n] @ [funObj]))
		)
		| Call -> FunError
		| Return -> (
			match stack with
			| [] -> FunError
			| head::tail -> (
				match arg_binding with
				| Name n -> IOFunOutRet (input_arg, (find_binding n bindings bindings), head)
				| _ -> FunError
			)
		)
		| FunEnd -> (
			match arg_binding with 
			| Name n -> IOFunOut (input_arg, (find_binding n bindings bindings))
			| _ -> print_string "WRONG\n"; FunError
		)
			
		| InOutFun (n, a) -> FunError
	);;

let call ((stack : const list), (bindings), (funBindings)) =
	match stack with
	| con::Name fun_name::tail -> (
		match con with
		|  Name arg_name -> (
			let n_bind = find_binding arg_name bindings bindings in 
			match n_bind with 
			| String s -> (
				let fun_commands = find_function fun_name funBindings in
				match fun_commands with
				| FunError -> FunError
				| Function (ab, c) -> execute_function (c, String s, ab, tail, [], bindings, funBindings)
				| InOutFunction (ab, c) -> execute_inout_function (c, String s, ab, con, tail, [], bindings @ [ab] @ [String s], funBindings)
				| _ -> FunError
			)
			| Num i -> (
				let fun_commands = find_function fun_name funBindings in
				match fun_commands with
				| FunError -> FunError
				| Function (ab, c) -> execute_function (c, Num i, ab, tail, [], bindings, funBindings)
				| InOutFunction (ab, c) -> execute_inout_function (c, Num i, ab, con, tail, [], bindings @ [ab] @ [Num i], funBindings)
				| _ -> FunError
			)
			| Unit -> (
				let fun_commands = find_function fun_name funBindings in
				match fun_commands with
				| FunError -> FunError
				| Function (ab, c) -> execute_function (c, Unit, ab, tail, [], bindings, funBindings)
				| InOutFunction (ab, c) -> execute_inout_function (c, Unit, ab, con, tail, [], bindings, funBindings)
				| _ -> FunError
			)
			| Boolean b -> (
				let fun_commands = find_function fun_name funBindings in
				match fun_commands with
				| FunError -> FunError
				| Function (ab, c) -> execute_function (c, Boolean b, ab, tail, [], bindings, funBindings)
				| InOutFunction (ab, c) -> execute_inout_function (c, Boolean b, ab, con, tail, [], bindings, funBindings)
				| _ -> FunError
			)
			| _ -> FunError
		)
		|  String s -> (
			let fun_commands = find_function fun_name funBindings in
				match fun_commands with
				| FunError -> FunError
				| Function (ab, c) -> execute_function (c, String s, ab, tail, [], bindings, funBindings)
				| InOutFunction (ab, c) -> execute_inout_function (c, String s, ab, con, tail, [], bindings, funBindings)
				| _ -> FunError
		)
		|  Num i -> (
			let fun_commands = find_function fun_name funBindings in
				match fun_commands with
				| FunError -> FunError
				| Function (ab, c) -> execute_function (c, Num i, ab, tail, [], bindings, funBindings)
				| InOutFunction (ab, c) -> execute_inout_function (c, Num i, ab, con, tail, [], bindings, funBindings)
				| _ -> FunError
		)
		| Unit -> (
			let fun_commands = find_function fun_name funBindings in
				match fun_commands with
				| FunError -> FunError
				| Function (ab, c) -> execute_function (c, Unit, ab, tail, [], bindings, funBindings)
				| InOutFunction (ab, c) -> execute_inout_function (c, Unit, ab, con, tail, [], bindings, funBindings)
				| _ -> FunError
		)
		| Boolean b -> (
			let fun_commands = find_function fun_name funBindings in
				match fun_commands with
				| FunError -> FunError
				| Function (ab, c) -> execute_function (c, Boolean b, ab, tail, [], bindings, funBindings)
				| InOutFunction (ab, c) -> execute_inout_function (c, Boolean b, ab, con, tail, [], bindings, funBindings)
				| _ -> FunError
		)
		| _ -> FunError
	)
	| _ -> FunError;;

let rec letStack ((commands : command list), (stack: const list), (bindings), (funBindings)) =
	match commands with
	| com::tail -> 
		print_string "-----Let Bindings-----\n";
		print_bindings bindings;
		print_string "-----Let Stack-----\n";
		print_stack stack;
		print_string "-------------------\n\n";
		print_string "-----Let Command------\n";
		print_string (commandToString com^"\n");
		(match com with
		| Push v ->  letStack (tail, (push v stack), bindings, funBindings)
		| Pop -> letStack (tail, (pop stack), bindings, funBindings)
		| Add -> letStack (tail, (add stack bindings), bindings, funBindings)
		| Sub -> letStack (tail, (sub stack bindings), bindings, funBindings)
		| Mul -> letStack (tail, (mul stack bindings), bindings, funBindings)
		| Div -> letStack (tail, (div stack bindings), bindings, funBindings)
		| Rem -> letStack (tail, (rem stack bindings), bindings, funBindings)
		| Neg -> letStack (tail, (neg stack bindings), bindings, funBindings)
		| Swap -> letStack (tail, (swap stack), bindings, funBindings)
		| ToString -> letStack (tail, (toString stack), bindings, funBindings)
		| PrintLn -> letStack (tail, stack, bindings, funBindings)
		| Quit -> Error
		| Cat -> letStack (tail, (cat stack bindings), bindings, funBindings)
		| And -> letStack (tail, (andOp stack bindings), bindings, funBindings)
		| Or -> letStack (tail, (orOp stack bindings), bindings, funBindings)
		| Not -> letStack (tail, (notOp stack bindings), bindings, funBindings)
		| Equal -> letStack (tail, (equalOp stack bindings), bindings, funBindings)
		| LessThan -> letStack (tail, (lessThan stack bindings), bindings, funBindings)
		| Bind -> letStack (tail, (bind stack bindings), (add_binding stack [] bindings), funBindings)
		| If -> letStack (tail, (ifOp stack bindings), bindings, funBindings)
		| Let -> 
			let commands_after_let = letCommands tail 0 in
			let let_result = letStack (tail, stack, bindings, funBindings) in
			letStack (commands_after_let, (push let_result stack), bindings, funBindings)
		| End -> 
			(match stack with
			| [] -> Error
			| head::tail -> head
			)
		| Fun (fun_name, fun_arg) -> 
			let fun_commands = getFunCommands ([], tail) in
			let commands_after_fun = getAfterFun tail in
			let funObj = Function (fun_arg, fun_commands) in
			letStack (commands_after_fun, (push Unit stack), bindings, (add_fun (FunName fun_name) funObj [] funBindings))
		| FunEnd -> letStack (tail, (push Error stack), bindings, funBindings)
		| Return -> letStack (tail, (push Error stack), bindings, funBindings)
		| Call -> let new_stack = (
			match stack with
			| x::y::end_stack -> end_stack
			| _ -> stack
		) in (
		let fun_call = call(stack, bindings, funBindings) in
		match fun_call with
		| FunOut frame -> letStack (tail, (push frame new_stack), bindings, funBindings)
		| IOFunOut (arg, new_bind) -> 
			print_string (constToString arg); print_string " : "; print_string (constToString new_bind); print_string "\n";
			let new_bindings = replace_binding arg new_bind [] bindings in
			letStack (tail, new_stack, new_bindings, funBindings)
		| IOFunOutRet (arg, new_bind, ret_val) ->
			let new_bindings = replace_binding arg new_bind [] bindings in
			letStack (tail, push ret_val new_stack, new_bindings, funBindings)
		| FunError -> 
			print_string "FUN ERROR\n";
			letStack (tail, (push Error stack), bindings, funBindings)
		| _ -> letStack (tail, (push Error stack), bindings, funBindings)
		)
		| InOutFun (fun_name, fun_arg) -> 
			let fun_commands = getFunCommands ([], tail) in
			let commands_after_fun = getAfterFun tail in
			let funObj = InOutFunction (fun_arg, fun_commands @ [FunEnd]) in
			letStack (commands_after_fun, (push Unit stack), bindings, (add_fun (FunName fun_name) funObj [] funBindings))
		)
	| _ -> Error;;

let rec print_fun_commands (commands) =
	match commands with
	| com::comTail -> 
		print_string "["; print_string (commandToString com); print_string "],";
		print_fun_commands comTail
	| [] -> print_string "\n";;

let rec print_functions (funBindings) =
	match funBindings with
	| FunName(Name fn)::Function(Name arg,com_list)::tail -> (
		print_string "RegFunction "; print_string fn; print_string " ("; print_string arg; print_string ") -> ";print_fun_commands com_list;
		print_functions tail
	)
	| FunName(Name fn)::InOutFunction(Name arg, com_list)::tail -> (
		print_string "InOutFunction "; print_string fn; print_string " ("; print_string arg; print_string ") -> ";print_fun_commands com_list;
		print_functions tail
	)
	| _ -> ();;

let rec printCommandList (command_list : command list) = 
	match command_list with
	| com::tail -> 
		print_string ("["^(commandToString com)^"],");
		printCommandList tail
	| [] -> print_string "\n";;

let interpreter ((input_file : string), (output_file : string)) : unit = 
	let ic = open_in input_file in
	let oc = open_out output_file in

	let rec loop_read acc =
		try 
				let l = input_line ic in loop_read (l::acc)
		with
		| End_of_file -> List.rev acc in

	let file_write s = Printf.fprintf oc "%s\n" s in

	let instructionStrings = loop_read [] in

	let rec printToOutput (stack : string list) =
		(* print_string "\n\nSTACK:\n"; *)
		match stack with
		| head::tail -> 
			(* print_string (head^"\n"); *)
			file_write head;
			printToOutput tail
		| [] -> () in

		let println (stack : const list) (bindings)=
		match stack with
		| [] -> push Error stack
		| x::newStack -> 
			(match x with
			| String s -> file_write s; newStack
			| Name n -> (
				let bindX = find_binding n bindings bindings in
				match bindX with 
				| String sb -> file_write sb; newStack
				| _ -> push Error stack
			)
			| _ -> push Error stack
			) in

		let rec stackToStrings ((stack : const list), (stringStack : string list)) =
			match stack with
			| value::tail -> 
				(match value with
				| Num i -> stackToStrings (tail, (stringStack @ [string_of_int i]))
				| String s -> stackToStrings (tail, (stringStack @ [s]))
				| Boolean b -> stackToStrings (tail, (stringStack @ [":"^(string_of_bool b)^":"]))
				| Error -> stackToStrings (tail, (stringStack @ [":error:"]))
				| Name n -> stackToStrings (tail, (stringStack @ [n]))
				| Unit -> stackToStrings (tail, (stringStack @ [":unit:"]))
				| None -> stackToStrings (tail, (stringStack @ [":none:"]))
				)
			| [] -> printToOutput stringStack
			in 
		
		let rec executeCommands ((commandList : command list), (stack : const list), bindings, funBindings) =
			(* print_string "\n\nCOMMAND LIST:\n"; *)
			match commandList with
			| com::tail -> 
				print_string "-----Bindings-----\n";
				print_bindings bindings;
				print_string "-----Functions-----\n";
				print_functions funBindings;
				print_string "-----Stack-----\n";
				print_stack stack;
				print_string "-------------------\n\n";
				print_string "-----Command------\n";
				print_string (commandToString com^"\n");
				(match com with
				| Push v -> executeCommands (tail, (push v stack), bindings, funBindings)
				| Pop -> executeCommands (tail, (pop stack), bindings, funBindings)
				| Add -> executeCommands (tail, (add stack bindings), bindings, funBindings)
				| Sub -> executeCommands (tail, (sub stack bindings), bindings, funBindings)
				| Mul -> executeCommands (tail, (mul stack bindings), bindings, funBindings)
				| Div -> executeCommands (tail, (div stack bindings), bindings, funBindings)
				| Rem -> executeCommands (tail, (rem stack bindings), bindings, funBindings)
				| Neg -> executeCommands (tail, (neg stack bindings), bindings, funBindings)
				| Swap -> executeCommands (tail, (swap stack), bindings, funBindings)
				| ToString -> executeCommands (tail, (toString stack), bindings, funBindings)
				| PrintLn -> executeCommands (tail, (println stack bindings), bindings, funBindings)
				| Quit -> stackToStrings (stack, [])
				| Cat -> executeCommands (tail, (cat stack bindings), bindings, funBindings)
				| And -> executeCommands (tail, (andOp stack bindings), bindings, funBindings)
				| Or -> executeCommands (tail, (orOp stack bindings), bindings, funBindings)
				| Not -> executeCommands (tail, (notOp stack bindings), bindings, funBindings)
				| Equal -> executeCommands (tail, (equalOp stack bindings), bindings, funBindings)
				| LessThan -> executeCommands (tail, (lessThan stack bindings), bindings, funBindings)
				| Bind -> executeCommands (tail, (bind stack bindings), (add_binding stack [] bindings), funBindings)
				| If -> executeCommands (tail, (ifOp stack bindings), bindings, funBindings)
				| Let -> 
					print_string "------LET------\n";
					let commands_after_end = letCommands tail 0 in
					let let_result = letStack (tail, stack, bindings, funBindings) in
					print_string "-----END LET------\n";
					executeCommands (commands_after_end, (push let_result stack), bindings, funBindings)
				| End -> ()
				| Fun (fun_name, fun_arg) -> 
					let fun_commands = getFunCommands ([], tail) in
					let commands_after_fun = getAfterFun tail in
					let funObj = Function (fun_arg, fun_commands) in
					executeCommands (commands_after_fun, (push Unit stack), bindings, (add_fun (FunName fun_name) funObj [] funBindings))
				| InOutFun (fun_name, fun_arg) ->
					let fun_commands = getFunCommands ([], tail) in
					let commands_after_fun = getAfterFun tail in
					let funObj = InOutFunction (fun_arg, fun_commands @ [FunEnd]) in
					executeCommands (commands_after_fun, (push Unit stack), bindings, (add_fun (FunName fun_name) funObj [] funBindings))
				| FunEnd -> ()
				| Return -> ()
				| Call -> let new_stack = (
					match stack with
					| x::y::end_stack -> end_stack
					| _ -> stack
				) in
					let fun_call = call(stack, bindings, funBindings) in
					match fun_call with 
					| FunOut None -> executeCommands (tail, new_stack, bindings, funBindings)
					| FunOut frame -> executeCommands (tail, (push frame new_stack), bindings, funBindings)
					| IOFunOut (arg, new_bind) -> 
						(* print_string (constToString arg); print_string " : "; print_string (constToString new_bind); print_string "\n"; *)
						let new_bindings = replace_binding arg new_bind [] bindings in
						executeCommands (tail, new_stack, new_bindings, funBindings)
					| IOFunOutRet (arg, new_bind, ret_val) ->
						let new_bindings = replace_binding arg new_bind [] bindings in
						executeCommands (tail, (push ret_val new_stack), new_bindings, funBindings)
					| FunError -> 
						print_string "FUN ERROR\n";
						executeCommands (tail, (push Error stack), bindings, funBindings)
					| _ -> executeCommands (tail, (push Error stack), bindings, funBindings)
					(* executeCommands (tail, (call (stack, bindings, funBindings)), bindings, funBindings) *)
				)
			| [] -> print_string "DONE\n";
				stackToStrings (stack, [])
				in

		let rec readLines (lineList : string list) (commandList : command list) =
			printCommandList commandList;
			match lineList with
			| [] -> executeCommands (commandList, [], [], [])
			| line::tail ->
				(* print_string (String.trim line); *)
				(let commandString : string list = Str.bounded_split (Str.regexp "[ ]") (String.trim line) 2 in
				match commandString with
				| "push"::value::[] -> print_string "push "; print_string (value); print_string"\n";
					(* Checks if value is Num *)
						(* "^[-][0-9]+$|^[0-9]+$" *)
					(if Str.string_match (Str.regexp "^[-]?[0-9]+$") value 0 
						then readLines tail (commandList @ [Push (Num (int_of_string value))])
					(* Checks if value is String *)
					else if Str.string_match (Str.regexp "\".*\"") value 0
						then readLines tail (commandList @ [Push (String (String.sub value 1 ((String.length value) - 2)))])
						(* then readLines tail (commandList @ [Push (String value)]) *)
					(* Checks if value is Name *)
					else if Str.string_match (Str.regexp "^[a-zA-Z_]+[a-zA-Z0-9_]*$") value 0
							then readLines tail (commandList @ [Push (Name value)])
					else match value with
					| ":true:" -> readLines tail (commandList @ [Push (Boolean true)])
					| ":false:" -> readLines tail (commandList @ [Push (Boolean false)])
					| ":error:" -> readLines tail (commandList @ [Push Error])
					| ":unit:" -> readLines tail (commandList @ [Push Unit])
					| _ -> readLines tail (commandList @ [Push Error])
					)
				(* | "fun"::funName::funArg::[] -> print_string "fun "; print_string funName; print_string funArg; print_string "\n";
					readLines tail (commandList @ [Fun ((Name funName), (Name funArg))]) *)
				| "fun"::input::[] -> print_string "fun "; print_string input; print_string "\n";
				let funInput = Str.bounded_split(Str.regexp "[ ]") input 2 in (
						match funInput with
					| funName::funArg::[] -> readLines tail (commandList @ [Fun ((Name funName), (Name funArg))])
					| _ -> readLines tail (commandList @ [Push Error])
				)
				| "inOutFun"::input::[] -> print_string "inOutFun "; print_string input; print_string "\n";
				let funInput = Str.bounded_split(Str.regexp "[ ]") input 2 in (
					match funInput with
					| funName::funArg::[] -> readLines tail (commandList @ [InOutFun ((Name funName), (Name funArg))])
					| _ -> readLines tail (commandList @ [Push Error])
				)
				| ["funEnd"] -> print_string "funEnd\n";
					readLines tail (commandList @ [FunEnd])
				| ["return"] -> print_string "return\n";
					readLines tail (commandList @ [Return])
				| ["call"] -> print_string "call\n";
					readLines tail (commandList @ [Call])
				| ["pop"] -> print_string "pop\n";
					readLines tail (commandList @ [Pop])
				| ["add"] -> print_string "add\n";
					readLines tail (commandList @ [Add])
				| ["sub"] -> print_string "sub\n";
					readLines tail (commandList @ [Sub])
				| ["mul"] -> print_string "mul\n";
					readLines tail (commandList @ [Mul])
				| ["div"] -> print_string "div\n";
					readLines tail (commandList @ [Div])
				| ["rem"] -> print_string "rem\n";
					readLines tail (commandList @ [Rem])
				| ["neg"] -> print_string "neg\n";
					readLines tail (commandList @ [Neg])
				| ["swap"] -> print_string "swap\n";
					readLines tail (commandList @ [Swap])
				| ["toString"] -> print_string "toString\n";
					readLines tail (commandList @ [ToString])
				| ["println"] -> print_string "println\n";
					readLines tail (commandList @ [PrintLn])
				| ["quit"] -> print_string "quit\n";
					readLines tail (commandList @ [Quit])
				| ["cat"] -> print_string "cat\n";
					readLines tail (commandList @ [Cat])
				| ["and"] -> print_string "and\n";
					readLines tail (commandList @ [And])
				| ["or"] -> print_string "or\n";
					readLines tail (commandList @ [Or])
				| ["not"] -> print_string "not\n";
					readLines tail (commandList @ [Not])
				| ["equal"] -> print_string "equal\n";
					readLines tail (commandList @ [Equal])
				| ["lessThan"] -> print_string "lessThan\n";
					readLines tail (commandList @ [LessThan])
				| ["bind"] -> print_string "bind\n";
					readLines tail (commandList @ [Bind])
				| ["if"] -> print_string "if\n";
					readLines tail (commandList @ [If])
				| ["let"] -> print_string "let\n";
				readLines tail (commandList @ [Let])
				| ["end"] -> print_string "end\n";
				readLines tail (commandList @ [End])
				| [other] -> print_string "{"; print_string other; print_string "}\n";
					readLines tail (commandList @ [Push Error])
				| other::value -> print_string "OTHER VALUE:"; print_string other; print_string "\n";
					readLines tail (commandList @ [Push Error])
				| [] -> print_string "NO COMMAND\n"
				)
in
readLines instructionStrings [];;

(* interpreter ("part3-input/input1.txt", "output/output1.txt");
interpreter ("part3-input/input2.txt", "output/output2.txt");
interpreter ("part3-input/input3.txt", "output/output3.txt");
interpreter ("part3-input/input4.txt", "output/output4.txt");
interpreter ("part3-input/input5.txt", "output/output5.txt"); *)
(* interpreter ("part3-input/input6.txt", "output/output6.txt"); *)
(* interpreter ("part3-input/input7.txt", "output/output7.txt");
interpreter ("part3-input/input8.txt", "output/output8.txt"); *)
(* interpreter ("part3-input/input9.txt", "output/output9.txt"); *)
(* interpreter ("part3-input/input10.txt", "output/output10.txt"); *)


	
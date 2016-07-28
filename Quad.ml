open Symbol
open Identifier
open Types
open Error
open Printf

module Q = Hashtbl.Make (
  struct
    type t = int
    let equal = (==)
    let hash = Hashtbl.hash
  end
)

type _operator =
	 UNIT
	|ENDU
	|PLUS
	|MINUS
	|MULT
	|DIV
	|MOD
	|ASSIGN
	|ARRAY
	|EQUAL
	|DIFFERENT
	|BIGGER
	|SMALLER
	|BIGGER_EQUAL
	|SMALLER_EQUAL
	|IFB
	|LABEL
	|JUMP
	|JUMPL
	|CALL
	|PAR
	|RET_OP	


and _pass_mode =
	 V
	|R
	|RET

and _operand =
    PTR of _operand
    |ADDRESS of Symbol.entry
    |LABEL_OP of int
    |INT_CONST of int
    |BYTE_CONST of char
    |STRING_LITERAL of string	(* check me *)
    |OBJECT of Symbol.entry
    |DOLLAR_DOLLAR			(* check me, anaferetai sto "$$" *)
    |PASSMODE of _pass_mode
    |UNKNOWN			(*	 *	*)
    |EMPTY				(*	 -	*)

and quadruple = {
	label: int;
	operator: _operator;
	mutable operand1: _operand;
	mutable operand2: _operand;
	mutable operand3: _operand
}

and  meta_info= {
	 meta_place: _operand;
	 meta_type: typ;
	 meta_next: int list;
	 meta_true: int list;
	 meta_false: int list
		    }

let quadTable = ref (Q.create 100)

let currentLabel = ref 0

let initQuadTable size = 
	quadTable := Q.create size;
	currentLabel := 0

let nextQuad () =
	!currentLabel + 1

let increaseCurrLabel () =
	currentLabel := !currentLabel + 1

let genQuad op x y z =
	increaseCurrLabel ();
	let newQuad = {
		label = !currentLabel;
		operator = op;
		operand1 = x;
		operand2 = y;
		operand3 = z
	} in
	Q.add !quadTable !currentLabel newQuad

let addressOf x = PTR x

let emptyList () = []

let merge lists =
	List.fold_left (@) [] lists

let backpatch l z =
	let replace quad = quad.operand3 <- (LABEL_OP z) in
	let temp = List.map (Q.find !quadTable) l in
	List.iter replace temp

let paramType subroutine_id n =
	try
	let subentr = lookupEntry subroutine_id LOOKUP_ALL_SCOPES true in
		match subentr.entry_info with
		 ENTRY_function func_info -> begin 
			let param = 
			  try 
			  List.nth func_info.function_paramlist (n-1) 
			  with _->
			    begin 
			    Error.error "bummer:list nth is NULL ";
			      {entry_id=(id_make "");entry_scope=(!currentScope);entry_info=ENTRY_none}
			    end 
		
			in
			  match param.entry_info with
				 ENTRY_parameter param_info ->
					param_info.parameter_type
				|_-> 
				   begin 
				      Error.error "BLAM: paramType called for not a parameter!"; 
				      TYPE_none 
				  end
			
		   end
		|_-> begin Error.error "BLAM: paramType called for not a function!"; TYPE_none end
	with Exit -> 	begin
			Error.error "This must not happen! parmType failed to lookupEntry!"; 
			TYPE_none
			end


let paramMode subroutine_id n =
	try
	let subentr = lookupEntry subroutine_id LOOKUP_ALL_SCOPES true in
		match subentr.entry_info with
		 ENTRY_function func_info -> begin 
			let param = 
			  try List.nth func_info.function_paramlist (n-1) 
			  with _->
			    begin 
			    Error.error "this is bammer cause the list is empty now";
			    {entry_id=(id_make "");entry_scope=(!currentScope);entry_info=ENTRY_none}
			    end 
			in
			  match param.entry_info with
			      ENTRY_parameter param_info -> begin
				  match param_info.parameter_mode with
				      PASS_BY_VALUE -> PASSMODE V
				    |PASS_BY_REFERENCE -> PASSMODE R
				end
			    |_-> begin Error.error "BLAM: paramMode called for not a parameter!"; EMPTY end
		   end
		  |_-> begin 
			Error.error "BLAM: paramMode called for not a function!"; 
			EMPTY 
		    end
	with Exit -> 	
	  begin
	    Error.error "This must not happen! paramMode failed to lookupEntry!"; 
	    EMPTY
	  end
	  
let isFunction program_id =
	try
	let progentr = lookupEntry program_id LOOKUP_ALL_SCOPES true in
		match progentr.entry_info with
			 ENTRY_function func_info ->
				if func_info.function_result = TYPE_proc
				then false
				else true
			|_-> begin Error.error "BLAM!!!! isFunction called for not a function"; false end
	with Exit -> 	begin
			Error.error "This must not happen! isFunction failed to lookupEntry"; 
			false
			end

let funcResult name =
	try
	let entr = lookupEntry name LOOKUP_ALL_SCOPES true in
		match entr.entry_info with
			 ENTRY_function func_info -> func_info.function_result
		|_-> begin Error.error "BLAM!!!!! funcResult called for not a function"; TYPE_none end
	with Exit -> 	begin
			Error.error "This must not happen! funcResult failed to lookupEntry"; 
			TYPE_none
			end

let sizeOf typos =
	Types.sizeOfType typos

	
let makeList x= [x]

let rec string_of_operand op q =
	match op with
	 |ADDRESS entry                 ->(match q.operator with
                                           |ARRAY-> id_name entry.entry_id
                                           |_-> "["^(id_name entry.entry_id)^"]"
                                          )
	 |LABEL_OP intl 		-> string_of_int intl
	 |INT_CONST int_const 		-> string_of_int int_const
	 |BYTE_CONST char_const 	-> begin match char_const with
                                                  '\n' -> "\\n"
                                                 |'\r' -> "\\r"
                                                 |'\t' -> "\\t"
                                                 |'\000' -> "\\0"
                                                 |_ -> String.make 1 char_const
                                            end
	|STRING_LITERAL string_literal 	-> string_literal
	|OBJECT entry   		-> (id_name entry.entry_id) ^ "~" ^ (string_of_int entry.entry_scope.sco_id)
	|DOLLAR_DOLLAR			-> "$$"
	|PASSMODE p -> begin
		match p with
		 V 			-> "V"
		|R 			-> "R"
		|RET 			-> "RET"
		end
	|UNKNOWN 			-> "*"
	|EMPTY 				-> "_"
        |_                              -> "Whot?"

let string_of_operator op = match op with
	 UNIT 		-> "unit"
	|ENDU 		-> "endu"
	|PLUS 		-> "+"
	|MINUS 		-> "-"
	|MULT 		-> "*"
	|DIV 		-> "/"
	|MOD 		-> "%"
	|ASSIGN 	-> ":="
	|ARRAY 		-> "array"
	|EQUAL 		-> "=="
	|DIFFERENT 	-> "!="
	|BIGGER 	-> ">"
	|SMALLER 	-> "<"
	|BIGGER_EQUAL 	-> ">="
	|SMALLER_EQUAL 	-> "<="
	|IFB 		-> "ifb"
	|LABEL 		-> "label"
	|JUMP 		-> "jump"
	|JUMPL 		-> "jumpl"
	|CALL 		-> "call"
	|PAR 		-> "par"
	|RET_OP 	-> "ret"	

let printQuad q =
	let label = string_of_int q.label in
	let operator = 	string_of_operator q.operator in
	let op1 = string_of_operand q.operand1 q in
	let op2 = string_of_operand q.operand2 q in
	let op3 = string_of_operand q.operand3 q in
	let string_quad =
		label ^ ": " ^
		operator ^ ", " ^
		op1 ^ ", " ^
		op2 ^ ", " ^
		op3 ^ "\n" 
	in
	printf "%s" string_quad
	
let printQuadTable () =
	(* 
	To currentLabel arxikopoieitai sto 0.
	Me to prwto genQuad pou tha treksei,
	prin ftiaxtei to prwto quad, to 
	currentLabel tha afksithei kai to neo
	quad tha parei to label 1.
	Ara to Quad Table ksekinaei arithmisi
	apo to 1.
	*)

	let i = ref 1 in
	let done_flag = ref false in

	while !done_flag = false do
		try
			let quad = Q.find !quadTable !i in
			begin
				printQuad quad;
				i := !i +1
			end
		with Not_found ->
			done_flag := true
	done

let string_list =  ref []

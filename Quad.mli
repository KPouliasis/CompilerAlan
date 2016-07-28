module Q :
  sig
    type key = int
    type 'a t
    val create : int -> 'a t
    val clear : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
  end


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
    |DOLLAR_DOLLAR		(* check me, anaferetai sto "$$" *)
    |PASSMODE of _pass_mode
    |UNKNOWN			(*	 *	*)
    |EMPTY			(*	 -	*)

and quadruple = {
	label: int;
	operator: _operator;
	mutable operand1: _operand;
	mutable operand2: _operand;
	mutable operand3: _operand
}
and  meta_info= {
	 meta_place: _operand;
	 meta_type: Types.typ;
	 meta_next: int list;
	 meta_true: int list;
	 meta_false: int list
		}

val quadTable           : quadruple Q.t ref
val initQuadTable	: int -> unit

val genQuad		: _operator -> _operand -> _operand -> _operand -> unit 

val addressOf		: _operand -> _operand
val nextQuad		: unit -> int

val emptyList		: unit -> int list
val merge		: int list list -> int list

val backpatch		: int list -> int -> unit

val paramType		: Identifier.id -> int -> Types.typ
val isFunction		: Identifier.id -> bool
val paramMode		: Identifier.id -> int -> _operand
val funcResult		: Identifier.id -> Types.typ

val sizeOf		: Types.typ -> int
val makeList		: int -> int list

val printQuad		: quadruple -> unit
val printQuadTable 	: unit -> unit

val string_list         : string list ref

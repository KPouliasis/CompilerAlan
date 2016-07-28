/* file: alanSyntax.mly */

%{

(* Header (Ocaml code)			*)
open Printf
open Format
open List
open Lexing
open Identifier
open Symbol
open Types
open Stack
open Parsing
open Quad
open Subroutine
  

type par_entry= {
           name: id;
           pass: pass_mode;
           par_type: typ;
	       }



let library_def=
  [(("writeInteger",TYPE_proc),[{name=id_make "n";pass=PASS_BY_VALUE;par_type=TYPE_int}]);
   (("writeString",TYPE_proc),[{name=id_make "msg";pass=PASS_BY_REFERENCE;par_type=TYPE_array (TYPE_byte,0)}]);
   (("writeByte",TYPE_proc),[{name=id_make "b";pass=PASS_BY_VALUE;par_type=TYPE_byte}]);
   
   (("writeChar",TYPE_proc),[{name=id_make "b";pass=PASS_BY_VALUE;par_type=TYPE_byte}]);
   
   (("readInteger",TYPE_int),[]);
   
   (("readByte",TYPE_byte),[]);
   
   (("readChar",TYPE_byte),[]);
   
   (("readString",TYPE_proc),[{name=id_make "n";pass=PASS_BY_VALUE;par_type=TYPE_int};
			     {name=id_make "s";pass=PASS_BY_REFERENCE;par_type=TYPE_array (TYPE_byte,0)}]);
   
   (("extend",TYPE_int),[{name=id_make "b";pass=PASS_BY_VALUE;par_type=TYPE_byte}]);
   
   (("shrink",TYPE_byte),[{name=id_make "b";pass=PASS_BY_VALUE;par_type=TYPE_int}]);
   
   (("strlen",TYPE_int),[{name=id_make "s";pass=PASS_BY_REFERENCE;par_type=TYPE_array (TYPE_byte,0)}]);

   (("strcmp",TYPE_int),[{name=id_make "s1";pass=PASS_BY_REFERENCE;par_type=TYPE_array (TYPE_byte,0)};
	      {name=id_make "s2";pass=PASS_BY_REFERENCE;par_type=TYPE_array (TYPE_byte,0)}]);

   (("strcpy",TYPE_proc),[{name=id_make "trg";pass=PASS_BY_REFERENCE;par_type=TYPE_array (TYPE_byte,0)};
	     {name=id_make "src";pass=PASS_BY_REFERENCE;par_type=TYPE_array (TYPE_byte,0)}]);

   (("strcat",TYPE_proc),[{name=id_make "s1";pass=PASS_BY_REFERENCE;par_type=TYPE_array (TYPE_byte,0)};
	      {name=id_make "s2";pass=PASS_BY_REFERENCE;par_type=TYPE_array (TYPE_byte,0)}])]




let rec add_params in_func paramlist  =
  match paramlist  with 
    |  []->endFunctionHeader in_func
    |hd::tl->ignore (newParameter hd.name hd.par_type hd.pass in_func true);add_params in_func tl




let rec lib_in_table lib_list=
  match lib_list
  with 
    | []->()
    |hd::tl->
       
       let s= newFunction (id_make (fst (fst hd))) true
       in 
	 begin 
	   openScope();
	   (add_params s (snd hd)) (snd (fst hd));   
	   closeScope ();
	   lib_in_table tl;
	 end

let () =
        (* This is done before parsing starts *)
        initSymbolTable 256;
        lib_in_table library_def;
        Error.message "alanc v0.98 Alan compiler"(*;
	Error.message "2006, Kostas Pouliasis, Michalis Famelis";
	Error.message "alanc is Free Software";
	Error.message "Distributed under the terms";
	Error.message "of the GNU Public License (GPL).";
	Error.message "In memory of Alan Turing.\n"*)
 
   
let nada = {meta_place=EMPTY; meta_type=TYPE_none; meta_next=[]; meta_true=[]; meta_false=[]}

let global =ref nada



let check_match  x y z=
    if  (x=z) && (x=y)
    then true
    else false

let type_stack = create ()
let id_stack=create ()
let if_stack=create()
let deref ptr= match ptr with
  |PTR(y)->y
  |_->ptr
let entry_stack=create()    
let flag=ref false


%}

/*
	Ocamlyacc declarations 
*/

/** reserved words 
**/
%token BYTE ELSE FALSE IF INT PROC REFERENCE RETURN WHILE TRUE

/** terminal symbols 
**/
%token L_SQ_BR R_SQ_BR L_CU_BR R_CU_BR L_BR R_BR 		 /* brackets  [ ] { } ( ) 	*/
%token COMMA COLON SEMICOLON 					 /* punctuation , : ; 		*/
%token ASSIGN PLUS MINUS MULT DIV MOD 				 /* operators = + - * / % 	*/
%token NOT AND OR 						 /* logic ! & | 		*/
%token EQUAL NOT_EQUAL SMALLER BIGGER SMALLER_EQUAL BIGGER_EQUAL /* comparison == != < > <= >= 	*/
%token NEWLINE
%token UPLUS UMINUS						 /* prosima + - 		*/

/** identifiers
**/
%token <string> ID

/** literals kai constants 
**/
%token <int> INT_CONST
%token <char> CHAR_CONST
%token <string> STRING_LITERAL

/** precedence 
**/
%left OR
%left AND
%nonassoc EQUAL NOT_EQUAL SMALLER BIGGER SMALLER_EQUAL BIGGER_EQUAL
%left PLUS MINUS
%left MULT DIV MOD
%left UPLUS UMINUS NOT

%start program
%type <unit> program


%%





/*** Grammar rules 
***/

/** <program> ::= <func-def>
**/
program:	func_def	{ }
;

/** <func-def> ::= <id> "(" [<fpar-list>] ")" ":" <r-type> (<local-def>)* <compound-stmt>
	<id> "(" [<fpar-list>] ")" ":" <r-type> (<local-def>)*	--> func_def_action  (middle action required)
**/
func_def:		func_def_action compound_stmt 
	  {
	    
	     
	     
	       
		begin 
		  backpatch $2.meta_next (nextQuad());
		  genQuad ENDU (OBJECT (pop entry_stack)) EMPTY EMPTY;
		  closeScope();
		  pop type_stack 
		end 
	    
	      
							      
						      }
;

/* <id> "(" [<fpar-list>] ")" ":" <r-type> (<local-def>)*	--> func_def_action  (middle action required)
	<id> 		--> id_action (middle action required)
	[fpar-list]	--> fpar-list-tmp
	(local-def)*	--> local-def-tmp */


func_def_action:	func_head_action local_def_tmp	
	  {
	        
		 genQuad UNIT  (OBJECT (top entry_stack)) EMPTY EMPTY
	    
	       
		 

	  }
							    



func_head_action:	id_action L_BR fpar_list_temp R_BR COLON r_type  
				{
				let create_pars in_func par_entry=
				  ignore (newParameter  par_entry.name  par_entry.par_type par_entry.pass in_func true);
				  ()
			        
				in                                                                            
				  openScope();  
				  List.iter (create_pars $1) $3;
                                  push $6 type_stack;
				  endFunctionHeader $1 $6
				    
				} 
;
                                
/* <id> --> id_action (middle action required)
*/

id_action:	ID	{ 
			  push $1 id_stack;
			 let f=newFunction (id_make $1) true in
			   begin
				(try
				  push (lookupEntry (id_make (top id_stack)) LOOKUP_ALL_SCOPES true) entry_stack
                        	with _ ->Error.error "buggg");
				  f
			   end
			}
;

/* [fpar-list]	--> fpar-list-tmp */
fpar_list_temp:	/* empty */	{ [] }
		|fpar_list	{ $1 }
;

/* (local-def)*	--> local-def-tmp */
local_def_tmp:	/* empty */			{ }
		| local_def_tmp local_def	{ }
;

/** <fpar-list> ::= <fpar-def> ("," <fpar-def>)*
	("," <fpar-def>)*	--> comma-fpar-def-tmp
**/
fpar_list:	fpar_def 			{ [$1] }
                |fpar_list COMMA fpar_def	{ List.append  $1 [$3] } 
;



/** <fpar-def> ::= <id> ":" ["reference"] <type>
	["reference"]	--> reference-tmp
**/
fpar_def:	ID COLON reference_tmp type_def  
	  {
            let id=(id_make $1) 
            in
            let check_valid_type () =
              match ($3,$4) with 
		|(PASS_BY_REFERENCE,_)
		|(PASS_BY_VALUE,TYPE_int)
		|(PASS_BY_VALUE,TYPE_byte)->()
		|_->
		   Error.error "Parameter cannot be declared as by Value unless it is of type int or byte"
            in check_valid_type () ;
	      {name=id;pass=$3;par_type=$4}
                
	  }
;

/* ["reference"]	--> reference-tmp */
reference_tmp:	/* empty */			{PASS_BY_VALUE }
		| REFERENCE	{PASS_BY_REFERENCE }
;

/** <data-type> ::= "int" | "byte"
**/
data_type:	  INT	{Types.TYPE_int }
    | BYTE	{Types.TYPE_byte}
;

/** <type> ::= <data-type> ["[" "]"]
	["[" "]"]	--> array-tmp
**/
type_def:	data_type array_tmp	{let define_type simple_type boolean=
					   match boolean 
					   with 
					   |false->simple_type
                                           |true->TYPE_array (simple_type,0)
					 in  (define_type $1 $2)
					}
;

/* ["[" "]"]	--> array-tmp */
array_tmp:	/* empty */		{false }
		| L_SQ_BR R_SQ_BR	{true }
;

/** <r-type> ::= <data-type> | "proc"
**/
r_type:		  data_type	{$1} /* $1 */
		 | PROC          {TYPE_proc} /*TYPE_proc */
;

/** <local-def> ::= <func-def> | <var-def>
**/
local_def:	  func_def	{ }
		| var_def	{ }
;

/** <var-def> ::= <id> ":" <data-type> ["[" <int-const> "]"] ";"
	["[" <int-const> "]"]	--> br-int-const-tmp
**/
var_def:	ID COLON data_type br_int_const_tmp SEMICOLON	{let define_type tuple alan_type=match fst tuple
								 with 
								   |false->alan_type 
								   |true->TYPE_array (alan_type,snd tuple)
								 in 
                                                                 newVariable (id_make $1) (define_type $4 $3) true
								}
;

/* ["[" <int-const> "]"]	--> br-int-const-tmp */
br_int_const_tmp:	/* empty */					{false,0}
			| L_SQ_BR INT_CONST  R_SQ_BR	{(true,$2)}
                                                             
			    
;


/** <stmt> ::= ";" | <l-value> "=" <expr> ";" | <compound-stmt> | <func-call> ";"
		| "if" "(" <cond> ")" <stmt> ["else" <stmt>]
		| "while" "(" <cond> ")" <stmt> | "return" [<expr>] ";"

	["else" <stmt>] --> else-stmt-tmp
	[<expr>]	--> return-stmt-tmp
**/
stmt:	 | SEMICOLON				{ nada } 
  | l_value ASSIGN expr SEMICOLON		
       { let check = (check_match $1.meta_type $3.meta_type $1.meta_type)in
	   begin 
	     (if (check)
	      then 
		  match $1.meta_type 
		  with 
		      TYPE_int
		    |TYPE_byte ->ignore(genQuad ASSIGN (deref $3.meta_place) EMPTY (deref $1.meta_place));
		    |_-> Error.error "expressions on both sides must be of simple type"
	      else  Error.error "type mismatch in assignment");		
	      
	      
	     nada
	   end   
	   } /*olo to xexname n apagorefsoume assign se string*/
  | compound_stmt				{$1 }
  | func_call SEMICOLON			{$1 }
      
  | if_cond stmt else_stmt_temp         { match $3 with 
					    |None ->{nada with meta_next=merge [(fst $1); (snd $1); $2.meta_next]}
					    |Some (x,y)->{nada with meta_next=
						  merge [x; y; $2.meta_next]}  
					       
					}
      
      
      
      

      
  | while_nonterm cond_while  stmt		{
						  begin
						    backpatch $3.meta_next $1;
						    genQuad JUMP EMPTY EMPTY (LABEL_OP $1);
						    {nada with meta_next=$2.meta_false}
						  end 
						    
						}
      
  | RETURN return_stmt_tmp SEMICOLON	
	    {
	      
	      let y=top type_stack in 
		if check_match $2.meta_type y $2.meta_type
		then 
		  begin   
		    if (y=TYPE_proc) 
		    then 
		      ()
		    else  		
		    genQuad ASSIGN (deref $2.meta_place) EMPTY DOLLAR_DOLLAR;
		    genQuad  RET_OP  EMPTY EMPTY EMPTY;
		    nada
		  end 
		else 
		  begin
		    ignore( Error.error "return type mismatch" );
		    nada
		  end
	    }

  ;
  
if_cond:  IF L_BR cond R_BR                     {backpatch $3.meta_true (nextQuad ());
						 push  $3 if_stack;
						 ($3.meta_false,[])
					       }

;
	
/* ["else" <stmt>] --> else-stmt-tmp */
else_stmt_temp:	/* empty */	{None }
		| else_action stmt 	{ Some ( $1,$2.meta_next)  }
;

else_action : ELSE {let l1=makeList (nextQuad ()) in
		      begin
		        genQuad JUMP EMPTY EMPTY UNKNOWN; 
		        backpatch (pop if_stack).meta_false  (nextQuad ());
		        l1
		      end 
                   }


;

while_nonterm:WHILE {nextQuad()}

;

cond_while:cond {backpatch $1.meta_true (nextQuad());$1}
;

/* [<expr>]	--> return-stmt-tmp */
return_stmt_tmp:	/* empty */	{ {nada with meta_type=TYPE_proc} }
			| expr		{ $1 }
;

/** <compound-stmt> ::= "{" (<stmt>)* "}"
	(<stmt>)*	--> cmp-stmt-tmp
**/
compound_stmt:	L_CU_BR cmp_stmt_tmp R_CU_BR	{$2 }
;

/* (<stmt>)*	--> cmp-stmt-tmp */
cmp_stmt_tmp:	        /*empty*/		{nada}
			
			| cmp_stmt_tmp2   {$1}
;


cmp_stmt_tmp2:          stmt          {{nada with meta_next= $1.meta_next}}
			|stmt1 stmt {{nada with meta_next=$2.meta_next} }

stmt1:     cmp_stmt_tmp2       {backpatch $1.meta_next (nextQuad ())}      

/** <func-call> ::= <id> "(" [<expr-list>] ")"
	[<expr-list>] --> expr-list-tmp
**/
func_call:	f_id  L_BR expr_list_tmp R_BR   
			  { 
			    if (List.length 
				  (try (Subrout.top_param_stuck ())
				   with Stack.Empty->
				     begin 
				       Printf.printf"no params maeet";
				       []
				     end)
				<> List.length $3)
			    then	
			      begin
				Error.error 
				  "Function %s called with %d  number of params but expects %d" 
				  $1 (List.length $3) (List.length (Subrout.top_param_stuck ()));
				ignore(Subroutine.pop_param_stuck ());
				ignore(Subroutine.pop_param_stuck ());
				nada
				  
			      end
			    else 
			      begin 
				(if (isFunction (id_make $1))
				 then 
				     
				     let w=newTemporary (funcResult (id_make $1))
				     in
				       begin
					 genQuad PAR (OBJECT w) (PASSMODE RET) EMPTY;
					 global:={meta_place= (OBJECT w);
						  meta_type=(funcResult (id_make $1));
						  meta_next=[];meta_true=[];meta_false=[]}
				       end 
					 
				 else global:=nada);
				ignore(Subroutine.pop_id ());
				ignore(Subroutine.pop_param_stuck ());
				try 
				  let e=lookupEntry (id_make $1) LOOKUP_ALL_SCOPES true
				  in 
				    genQuad  CALL EMPTY EMPTY (OBJECT  e);
				    (!global)
				with 
				    Exit->
				      Error.error "this should not happen:error in func_call";
				      (!global)
			      end 
				
				
				
			  }
;

f_id:  ID  { 
	     Subrout.push_id (id_make $1);
	 
	     Subrout.push_param_stuck (Subrout.check_for_fun (id_make $1));
	       $1
	   }
;


/* [<expr-list>] --> expr-list-tmp */
expr_list_tmp:	/* empty */	{ [] }
		| expr_list	{ $1 }
;

/** <expr-list> ::= <expr> ("," <expr>)*
	("," <expr>)*	--> expr-seq-tmp
**/
expr_list:	 expr	{if (Subrout.match_nth_params 1 (Subrout.top_param_stuck ()) $1)
			 then 
			     begin
			     genQuad PAR (deref $1.meta_place) (paramMode (Subrout.top_id ()) 1) EMPTY ;
			       [$1]
			     
			     end 
			 else 
			     begin
			     Error.error "mismatch in fst parameter";
			     []
			     end 
			     } 
		|expr_list COMMA expr 
		    {if (Subrout.match_nth_params ((List.length $1)+1) (Subrout.top_param_stuck ()) $3)
		     then 
			 begin
			   genQuad PAR (deref $3.meta_place) (paramMode (Subrout.top_id ()) ((List.length $1)+1)) EMPTY ;
			   
			   List.append  $1 [$3]
			 end 
		     else 
			 begin 
			 Error.error "parameter mismatch";
			 []
			 end 
			 }
					
					
;



/** <expr> ::= <int-const> | <char-const> | <l-value> | "(" <expr> ")" | <func-call>
		| ("+"|"-") <expr> | <expr> ("+"|"-"|"*"|"/"|"%") <expr>
	("+"|"-") <expr>	--> "+" <expr> | "-" expr
	<expr> ("+"|"-"|"*"|"/"|"%") <expr>	--> <expr> "+" <expr> | <expr> "-" <expr> |.....
**/
expr:	  	| INT_CONST			
                    { {meta_type=TYPE_int; meta_place=INT_CONST $1; meta_next=[]; meta_true=[]; meta_false=[]}  }
		| CHAR_CONST			
		    { {meta_type=TYPE_byte;meta_place=BYTE_CONST $1; meta_next=[]; meta_true=[]; meta_false=[]} }
		| l_value			{ $1 }
		| L_BR expr R_BR		{ {$2 with meta_place=(deref $2.meta_place)}  }
		| func_call	 		{ $1 } 
		    
		| PLUS expr %prec UPLUS		{let check=check_match $2.meta_type TYPE_int TYPE_int in
						   if check
						   then    {$2 with meta_place=(deref $2.meta_place)}  
						   else	
						     begin 
						       Error.error "type mismatch";
						       nada
						     end
						} 
      		| MINUS expr %prec UMINUS	
		    {let check=check_match $2.meta_type  TYPE_int TYPE_int in
		       if check 
		       then	
			 begin 
			   let w = newTemporary TYPE_int in 
			     begin
			       ignore (genQuad MINUS (deref $2.meta_place) EMPTY (OBJECT w) );
			       {meta_place = OBJECT w; meta_type=TYPE_int; 
				meta_next=[]; meta_true=[]; meta_false=[]
			       }
			     end 
			 end
		       else	
			 begin
			   Error.error "type mismatch";
			   nada
			 end
		    }
		| expr PLUS expr		
		    {let check=check_match $1.meta_type $3.meta_type TYPE_int 
		     in
		       if check 
		       then	begin 
			   let w = (newTemporary TYPE_int)
			   in
			     begin
			       ignore ( genQuad PLUS (deref $1.meta_place)    
                                          (deref $3.meta_place) (OBJECT w) );
			       {meta_place = OBJECT w; meta_type=TYPE_int;
				meta_next=[]; meta_true=[]; meta_false=[]
							   }
			     end
			 end 
		       else	
			 begin
			   Error.error "type mismatch";
			   nada
			 end 
		    }
		| expr MINUS expr		{let check=check_match $1.meta_type $3.meta_type TYPE_int in
						   if check 
						   then	begin 
						       let w = newTemporary TYPE_int in
							 begin
							   ignore ( genQuad MINUS (deref $1.meta_place) 
								      (deref $3.meta_place) (OBJECT w) );
							   {meta_place = OBJECT w; meta_type=TYPE_int;
							    meta_next=[]; meta_true=[]; meta_false=[]
							   }
							 end
						     end 
						   else	begin
						       Error.error "type mismatch";
						       nada
						     end 
						}
		| expr MULT expr		{let check=check_match $1.meta_type $3.meta_type TYPE_int in
							if check 
							then	begin 
								let w = newTemporary TYPE_int in
									begin
									ignore 
									 ( genQuad MULT (deref $1.meta_place) 
									     (deref $3.meta_place) (OBJECT w) );
									{meta_place = OBJECT w; meta_type=TYPE_int;
									 meta_next=[]; meta_true=[]; meta_false=[]
									}
									end
								end 
							else	begin
								Error.error "type mismatch";
								nada
								end 
						}
		| expr DIV expr			{let check=check_match $1.meta_type $3.meta_type TYPE_int in
							if check 
							then	begin 
								let w = newTemporary TYPE_int in
									begin
									ignore ( genQuad DIV (deref $1.meta_place) 
									(deref $3.meta_place) (OBJECT w) );
									
									  {meta_place = OBJECT w; meta_type=TYPE_int;
									   meta_next=[]; meta_true=[]; meta_false=[]
									  }
									end
								end 
							else	begin
								Error.error "type mismatch";
								nada
								end 
						}
		| expr MOD expr			{let check=check_match $1.meta_type $3.meta_type TYPE_int in
							if check 
							then	begin 
								let w = newTemporary TYPE_int in
									begin
									ignore 
									  ( genQuad MOD 
									      (deref $1.meta_place) (deref $3.meta_place)
									      (OBJECT w) );
									  {meta_place = OBJECT w; meta_type=TYPE_int;
									   meta_next=[]; meta_true=[]; meta_false=[]
									  }
									end
								end 
							else	begin
								ignore(Error.error "type mismatch");
								nada
								end 
						}
;

/** <l-value> ::= <id> [ "[" <expr> "]" ] | <string-literal>
	[ "[" <expr> "]" ]	--> br-expr-tmp
**/
l_value:	  ID br_expr_tmp	
	  {
	    let id = id_make $1 in
	    let e = lookupEntry id LOOKUP_ALL_SCOPES true in
	      match $2 with
		  None -> begin 
		      match e.entry_info with
			  ENTRY_variable var_info->
			    { meta_place = PTR (OBJECT e); meta_type=var_info.variable_type;
			      meta_next=[]; meta_true=[]; meta_false=[]} (*TODO: checkme an thelei PTR *)
			|ENTRY_parameter par_info->
			   { meta_place = PTR (OBJECT e); meta_type=par_info.parameter_type;
			     meta_next=[]; meta_true=[]; meta_false=[]} (*TODO: checkme an thelei PTR*)
			|_-> 	begin
			      ignore(Error.error "This should not happen: false entry as l-value.");
			      nada
			  end
		    end
		|Some stuff -> 	
		   begin
		     match e.entry_info 
		     with
			 ENTRY_variable var_info ->
			   begin 
			     match var_info.variable_type with
			     |TYPE_array (typos, _)-> (
			           (* PAPAAAAAAAAAAAAAATZAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA*)
			           if (typos = TYPE_byte) then ignore (newTemporary TYPE_byte) else ();
				   let w = newTemporary typos in
				   let index = stuff.meta_place in
				     begin
				       ignore(
					       genQuad ARRAY (OBJECT e) (deref index) (ADDRESS w));
				       {meta_place=PTR (ADDRESS w); meta_type=typos;
					meta_next=[]; meta_true=[]; meta_false=[]}
				     end
		             )
			     |_-> begin
				     ignore(Error.error "This should not happen");
				     nada
				  end	
			   end
		       |ENTRY_parameter par_info ->
			  if par_info.parameter_mode = PASS_BY_REFERENCE 
			  then
			    begin 
			      match par_info.parameter_type 
			      with
				  TYPE_array (typos, _)-> (
			           (* PAPAAAAAAAAAAAAAATZAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA*)
			           if (typos = TYPE_byte) then ignore (newTemporary TYPE_byte) else ();
				    let w = newTemporary typos in
				    let index = stuff.meta_place in
				      begin
					ignore(
						genQuad ARRAY (OBJECT e) (deref index) (ADDRESS w));
					{meta_place=(PTR (ADDRESS w)); meta_type=typos;
					 meta_next=[]; meta_true=[]; meta_false=[]}
				      end
				      )
				|_-> begin
				      ignore(Error.error "This should not happen.");
				      nada
				  end
			    end
			  else	
			    begin
			      ignore(Error.error "This should not happen");
			      nada
			    end
			      
		       |_-> 	
			  begin
			    ignore(Error.error "This should not happen");
			    nada
			  end
		   end
		     (*|_-> 	begin
		       ignore(Error.error "This should not happen.");
							nada
		       end*)
	  }
		| STRING_LITERAL	
		    { string_list := $1 :: !string_list;
		      {meta_place=PTR (STRING_LITERAL $1); meta_type=(TYPE_array (TYPE_byte, ((String.length $1) + 1)));
		       meta_next=[]; meta_true=[]; meta_false=[]}
		    }
;

/* [ "[" <expr> "]" ]	--> br-expr-tmp */
br_expr_tmp:	/* empty */		{ None }
		| L_SQ_BR expr R_SQ_BR	{
					  if ($2.meta_type=TYPE_int) 
					  then Some $2
					  else	
					    begin
					      ignore(Error.error "integer expression expected in brackets");
					      None
					    end
					}
;

/** <cond> ::= "true" | "false" | "(" <cond> ")" | "!" <cond>
		| <expr> ("==" | "!=" | "<" | ">" | "<=" | ">=") <expr>
		| <cond> ("&" | "|") <cond>
**/
cond:	cond_minus                      {$1} 

	|  fstand cond_minus		
				    	{let and_cond_true =
						{nada with meta_true=$2.meta_true  } 
				    	 in
				    	 let and_cond=
					 	{and_cond_true with
							meta_false=merge [$1.meta_false; $2.meta_false]} 
				    	 in
					 and_cond
					}
	| fstor cond_minus          	{let or_cond_true =
						{nada with
							meta_true = merge [$1.meta_true; $2.meta_true] }
					 in
				    	 let or_cond =
					 	{or_cond_true with
							meta_false=$2.meta_false} 
					  in
					  or_cond
					}

;

fstor :cond OR				{backpatch $1.meta_false (nextQuad());$1}
;

fstand:  cond AND                   	{backpatch $1.meta_true (nextQuad());$1}
;

cond_minus:	  TRUE			{
					 let info = {nada with meta_true=makeList ( nextQuad() ) } in
					 begin  ignore ( genQuad JUMP EMPTY EMPTY UNKNOWN );
						info
					 end
					}
	| FALSE				{ 
					 let info = {nada with meta_false=makeList ( nextQuad() ) } in
					 begin  ignore ( genQuad JUMP EMPTY EMPTY UNKNOWN );
						info
					 end
					}
	| L_BR cond R_BR		{ $2 }
	| NOT cond			{ {meta_place=nada.meta_place; meta_type=nada.meta_type;
					   meta_next=[];meta_true=$2.meta_false;meta_false=$2.meta_true}
					}
	| expr EQUAL expr		{
					  let check = (check_match $1.meta_type $3.meta_type TYPE_int) 
							||  
						     (check_match $1.meta_type $3.meta_type TYPE_byte)
					 in if check 
					   then
					     let info = { nada with meta_true=makeList (nextQuad ()) } in		
					     let () = genQuad EQUAL (deref $1.meta_place) (deref $3.meta_place) UNKNOWN 
					     in
					     let new_info = { info with meta_false = makeList (nextQuad()) } in
		  			     let ()=genQuad JUMP EMPTY EMPTY UNKNOWN in
					       new_info 
					   else 	
					     begin 
					       ignore ( Error.error "Expressions in comparisons must have type Int or Byte" );
					       nada 
					     end
					}
	| expr NOT_EQUAL expr		
	    {
	      let check = (check_match $1.meta_type $3.meta_type TYPE_int) 
		||  
		(check_match $1.meta_type $3.meta_type TYPE_byte)
	      in 
		if check then
		  let info = { nada with meta_true=makeList (nextQuad ()) } in		
		  let () = genQuad DIFFERENT (deref $1.meta_place) (deref $3.meta_place) UNKNOWN in
		  let new_info = { info with meta_false = makeList (nextQuad()) } in
		  let ()=genQuad JUMP EMPTY EMPTY UNKNOWN in
		    new_info 
		else 	
		  begin 
		    ignore ( Error.error "Expressions in comparisons must have type Int or Byte" );
		    nada 
		  end
					}
	| expr SMALLER expr		
	    {
	      let check = (check_match $1.meta_type $3.meta_type TYPE_int) 
		||  
		(check_match $1.meta_type $3.meta_type TYPE_byte)
	      in if check then
		  let info = { nada with meta_true=makeList (nextQuad ()) } in		
		  let () = genQuad SMALLER (deref $1.meta_place)(deref $3.meta_place) UNKNOWN in
		  let new_info = { info with meta_false = makeList (nextQuad()) } in
		  let ()=genQuad JUMP EMPTY EMPTY UNKNOWN in
		    new_info 
		else 	
		  begin 
		    ignore ( Error.error "Expressions in comparisons must have type Int or Byte" );
		    nada 
		  end
	    }
	| expr BIGGER expr		
	    {
	      let check = (check_match $1.meta_type $3.meta_type TYPE_int) 
		||  
		(check_match $1.meta_type $3.meta_type TYPE_byte)
	      in if check 
		then
		  let info = { nada with meta_true=makeList (nextQuad ()) } in		
		  let () = genQuad BIGGER (deref $1.meta_place) (deref $3.meta_place) UNKNOWN in
		  let new_info = { info with meta_false = makeList (nextQuad()) } in
		  let ()=genQuad JUMP EMPTY EMPTY UNKNOWN in
		    new_info 
		else 	
		  begin 
		    ignore ( Error.error "Expressions in comparisons must have type Int or Byte" );
		    nada 
		  end
            }
	| expr SMALLER_EQUAL expr	
	    {
	      let check = (check_match $1.meta_type $3.meta_type TYPE_int) 
		||  
		(check_match $1.meta_type $3.meta_type TYPE_byte)
	      in if check then
		  let info = { nada with meta_true=makeList (nextQuad ()) } in		
		  let () = genQuad SMALLER_EQUAL (deref $1.meta_place) (deref  $3.meta_place) UNKNOWN in
		  let new_info = { info with meta_false = makeList (nextQuad()) } in
		  let ()=genQuad JUMP EMPTY EMPTY UNKNOWN in
		    new_info 
		else 	
		  begin 
		    ignore ( Error.error "Expressions in comparisons must have type Int or Byte" );
		    nada 
		  end
	    }
	| expr BIGGER_EQUAL expr	
	    {
	      let check = (check_match $1.meta_type $3.meta_type TYPE_int) 
		||  
		(check_match $1.meta_type $3.meta_type TYPE_byte)
	      in if check then
		  let info = { nada with meta_true=makeList (nextQuad ()) } in		
		  let () = genQuad BIGGER_EQUAL (deref $1.meta_place) (deref $3.meta_place) UNKNOWN in
		  let new_info = { info with meta_false = makeList (nextQuad()) } in
		  let ()=genQuad JUMP EMPTY EMPTY UNKNOWN in
		    new_info 
		else 	begin 
		    ignore ( Error.error "Expressions in comparisons must have type Int or Byte" );
		    nada 
		  end
	    }

;

%%


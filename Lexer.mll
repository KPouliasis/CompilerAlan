{
open Printf
open Lexing
open Parser 

exception No_valid_token of string*int*int


let incr_linenum lexbuf =
	let pos = lexbuf.lex_curr_p in
	lexbuf.lex_curr_p <- { pos with
		pos_lnum = pos.pos_lnum + 1;
		pos_bol = pos.pos_cnum;
	
			     }
let lex_error lexbuf msg =
	let pos = lexbuf.lex_curr_p in
	raise (No_valid_token (msg, pos.pos_lnum, pos.pos_cnum - pos.pos_bol))


let hex_to_decimal hex_digit=
  if (Char.code hex_digit <58 )
  then int_of_string (String.make 1 hex_digit)
  else (10+ (Char.code (Char.uppercase hex_digit) -65))


let rec  extract_char lexeme = 
  if (String.length lexeme >1) 
  then 
    match lexeme with 
      |"\\n" ->CHAR_CONST '\n'
      |"\\r" ->CHAR_CONST '\r'
      |"\\t"  ->CHAR_CONST '\t'
      |"\\0" ->CHAR_CONST '\000'
      |"\\'" ->CHAR_CONST '\''
      |"\\\"" ->CHAR_CONST '"'
      |"\\\\" -> CHAR_CONST '\\'
      |(*hex_seq->extract_char (String.escaped String.hex_seq.[)*)  
	 str  ->CHAR_CONST (Char.chr (16*(hex_to_decimal str.[2]) + hex_to_decimal str.[3])) 
	     


(*( (Char.escaped str.[2])) * 16 +(int_of_string (Char.escaped  str.[3]))
	    in (CHAR_CONST ((string_of_int  hex_to_int).[0]))*) 
  else CHAR_CONST (lexeme.[0])

     
}
			     
let digit=['0'-'9']
let letter=['a'-'z' 'A'-'Z']
let hex_digit=['0'-'9''A'-'F' 'a'-'f']
let identifier=['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let sign =['+' '-']
let printable_allowed_const='~'|'`'|'@'|'#'|'$'|'%'|'^'|'?'|'!'|'.'|','|'_'
  |'('|')'|'{'|'}'|'['|']'|':'|';'|'='|'+'|'*'|'&'|'-'|'/'|' ' | '>' | '<'
let escape_conseq=("\\x" hex_digit hex_digit)| "\\n"| "\\t"| "\\r"| "\\t"| "\\0"| "\\'"| "\\\"" | "\\\\"

rule alan_parse = parse
|"--"		{ simple_comment lexbuf }
|"(*"		{ comments 0 lexbuf  }
|'\n'	  	{ incr_linenum lexbuf; alan_parse lexbuf }
|' '|'\t'|'\r'	{ alan_parse lexbuf }
|'''( _ as char) '''  	{ CHAR_CONST char }
(*|"\\x" (hex_digit as h1) (hex_digit as h2)  {let hex1=Char.escaped h1 in 
                                             let hex2=Char.escaped h2 in 
                                             let word="x"^hex1^hex2 in 
                                             let asci_code=int_of_string word in
                                               CHAR_CONST (char_of_int asci_code) }
|"\\\""         {CHAR_CONST '"'}*)
|'\'' ((printable_allowed_const|escape_conseq|letter|digit) as char_lexeme)'\'' 
      {extract_char char_lexeme}
|"while"        {WHILE}
|"if"	 	{ IF }
|"else"		{ ELSE }
|"reference"    {REFERENCE}
|"byte"         {BYTE}
|"proc"		{ PROC }
|"true"		{ TRUE }
|"false"	{ FALSE }
|"int"          {INT}
|"return"       {RETURN}
|'['		{ L_SQ_BR }
|']'		{ R_SQ_BR }
|'{'		{ L_CU_BR }
|'}'		{ R_CU_BR }
|'('		{ L_BR }
|')'		{ R_BR }
|','		{ COMMA }
|':'		{ COLON }
|';'		{ SEMICOLON }
|digit+ as arithm		{ INT_CONST  (int_of_string arithm) } (* etsi ta int constants einai mono akeraioi*)
|identifier as identifier	{ ID identifier }

|'"'(((printable_allowed_const|escape_conseq|letter|digit)* )as word) '"' 	{ STRING_LITERAL word }
|'='		{ ASSIGN }
|'+'		{ PLUS }
|'-'		{ MINUS }
|'*'		{ MULT }
|'/'		{ DIV }
|'%'		{ MOD }
|'!'		{ NOT }
|'&'		{ AND }
|'|'		{ OR }
|"=="		{ EQUAL }
|"!="		{ NOT_EQUAL }
|'<'		{ SMALLER }
|'>'		{ BIGGER }
|"<="		{ SMALLER_EQUAL }
|">="		{ BIGGER_EQUAL }
| eof 		{ raise End_of_file }
|_		{ lex_error lexbuf "Unknown token:" }	
  
and simple_comment=parse
|'\n' 		{ incr_linenum lexbuf;alan_parse lexbuf }
| _   		{ simple_comment lexbuf }
| eof 		{ lex_error lexbuf "Unfinished comment:" }

and comments level = parse
| "*)"		{ if level = 0 then alan_parse lexbuf
		else comments (level-1) lexbuf
		}
| "(*"		{ comments (level+1) lexbuf }
| '\n'		{ incr_linenum lexbuf; comments level lexbuf }
| _		{ comments level lexbuf }
| eof		{ lex_error lexbuf "Comments are not closed:" }



open Lexing
open Parser
open Lexer
open Printf
open Symbol
open Quad

(* 
 * read_file 
 * by Markus Mottl 
 *  
 * http://alan.petitepomme.net/cwn/2004.03.23.html#2 
 *
 * 
 * Function "read_file" does the obvious: read a file as fast as possible
 * into a string.
 *
 * Function "read_channel" reads a channel of unbounded size (as long as the
 * maximum string length is not exceeded, of course).  It also takes the
 * optional argument "buf_size", which you can set depending on the kind
 * of channel you read from (the default 4096 bytes are somewhat optimal
 * when reading from files on Linux).
 *
 *)

(* from here *)
let rec copy_lst res ofs = function
  | [] -> res
  | (str, len) :: t ->
      let pos = ofs - len in
      String.unsafe_blit str 0 res pos len;
      copy_lst res pos t

let read_channel ?(buf_size = 4096) =
  let rec loop len lst ch =
    let buf = String.create buf_size in
    let n = input ch buf 0 buf_size in
    if n <> 0 then loop (len + n) ((buf, n) :: lst) ch
    else copy_lst (String.create len) len lst in
  loop 0 []

let read_file name =
  let file = open_in name in
  let size = in_channel_length file in
  try
    let buf = String.create size in
    really_input file buf 0 size;
    close_in file;
    buf
  with exc ->
    (try close_in file with _ -> ());
    raise exc
(* up to here *)

 let rec syntax lexbuf =
		program alan_parse lexbuf;
		syntax lexbuf

let input_file_name = ref "name"

let main () =
	try
	   (*let input_file_name = Sys.argv.(1) in*)
	   input_file_name := Sys.argv.(1);
	   let inp = read_file !input_file_name in
	   let lexbuf = Lexing.from_string inp in
	   lexbuf.lex_curr_p <- {
		pos_fname = !input_file_name;
		pos_lnum = 1;
		pos_bol = 0;
		pos_cnum = 0
	   } ;
           try 	
                   syntax lexbuf;
           with 
             |Parsing.Parse_error->
                let pos=lexbuf.lex_curr_p in
                printf  "syntax error %d %d \n" pos.pos_lnum (pos.pos_cnum -pos.pos_bol)        

        with End_of_file -> 
                begin
                	if !Error.numErrors = 0 then 
                		begin
                        (*printQuadTable ();*)
                        Finalcode.make_asm !input_file_name
                        end
                    else begin
                        Error.message "Total %d errors. Exiting..." !Error.numErrors;
                        exit 1
                   		end                    	
                        
                end
	  |No_valid_token (x,y,z)-> printf "%s line %d, character %d\n" x y z
	  |Invalid_argument("Array.get") -> print_string "No such file.\n"
	  |Invalid_argument("index out of bounds") -> print_string "Please specify input file.\n"
	  |Sys_error s -> print_string "No such file.\n"
          |Exit -> begin 
                        Error.message "Total %d errors. Exiting..." !Error.numErrors;
                        exit 1
                   end
          

let _ = Printexc.print main ()



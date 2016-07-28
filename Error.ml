open Format
open Lexing
open Parsing

exception Terminate

type verbose = Vquiet | Vnormal | Vverbose

(* diko mas *)
let line_pos () = 
	let position = Parsing.symbol_start_pos () in
	position.pos_lnum
 
let char_pos () =
	let position = Parsing.symbol_start_pos () in
	position.pos_cnum - position.pos_bol
(* /diko mas *)

let flagVerbose = ref Vnormal

let numErrors = ref 0
let maxErrors = ref 10
let flagWarnings = ref true
let numWarnings = ref 0
let maxWarnings = ref 200

type position =
    PosPoint   of Lexing.position
  | PosContext of Lexing.position * Lexing.position
  | PosDummy

let position_point lpos = PosPoint lpos
let position_context lpos_start lpos_end = PosContext (lpos_start, lpos_end)
let position_dummy = PosDummy

let print_position ppf pos =
  match pos with
  | PosPoint lpos ->
      fprintf ppf "@[file \"%s\",@ line %d,@ character %d:@]@ "
        lpos.pos_fname lpos.pos_lnum (lpos.pos_cnum - lpos.pos_bol)
  | PosContext (lpos_start, lpos_end) ->
      if lpos_start.pos_fname != lpos_end.pos_fname then
        fprintf ppf "@[file \"%s\",@ line %d,@ character %d to@ \
                     file %s,@ line %d,@ character %d:@]@ "
          lpos_start.pos_fname lpos_start.pos_lnum
          (lpos_start.pos_cnum - lpos_start.pos_bol)
          lpos_end.pos_fname lpos_end.pos_lnum
          (lpos_end.pos_cnum - lpos_end.pos_bol)
      else if lpos_start.pos_lnum != lpos_end.pos_lnum then
        fprintf ppf "@[file \"%s\",@ line %d,@ character %d to@ \
                     line %d,@ character %d:@]@ "
          lpos_start.pos_fname lpos_start.pos_lnum
          (lpos_start.pos_cnum - lpos_start.pos_bol)
          lpos_end.pos_lnum
          (lpos_end.pos_cnum - lpos_end.pos_bol)
      else if lpos_start.pos_cnum - lpos_start.pos_bol !=
              lpos_end.pos_cnum - lpos_end.pos_bol then
        fprintf ppf "@[file \"%s\",@ line %d,@ characters %d to %d:@]@ "
          lpos_start.pos_fname lpos_start.pos_lnum
          (lpos_start.pos_cnum - lpos_start.pos_bol)
          (lpos_end.pos_cnum - lpos_end.pos_bol)
      else
        fprintf ppf "@[file \"%s\", line %d, character %d:@]@ "
          lpos_start.pos_fname lpos_start.pos_lnum
          (lpos_start.pos_cnum - lpos_start.pos_bol)
  | PosDummy ->
      ()

let no_out buf pos len = ()
let no_flush () = ()
let null_formatter = make_formatter no_out no_flush

let internal_raw (fname, lnum) fmt =
  let fmt = "@[<v 2>" ^^ fmt ^^ "@]@;@?" in
  incr numErrors;
  let cont ppf =
    raise Terminate in
  eprintf "Internal error occurred at %s:%d,@ " fname lnum;
  kfprintf cont err_formatter fmt

and fatal fmt =
  let fmt = "@[<v 2>Fatal error: " ^^ fmt ^^ "@]@;@?" in
  incr numErrors;
  let cont ppf =
    raise Terminate in
  begin
  eprintf "line %d" (line_pos ());
  eprintf ", character %d -> " (char_pos ());
  kfprintf cont err_formatter fmt
  end

and error fmt =
  let fmt = "@[<v 2>Error: " ^^ fmt ^^ "@]@;@?" in
  incr numErrors;
  if !numErrors >= !maxErrors then
    let cont ppf =
      eprintf "Too many errors, aborting...\n";
      raise Terminate in
    kfprintf cont err_formatter fmt
  else
	begin 	
	eprintf "line %d" (line_pos ());
	eprintf ", character %d \n\t" (char_pos ());
	eprintf fmt
	end

and warning fmt =
  let fmt = "@[<v 2>Warning: " ^^ fmt ^^ "@]@;@?" in
  if !flagWarnings then
  begin
    incr numWarnings;
    if !numWarnings >= !maxWarnings then
      let cont ppf =
        eprintf "Too many warnings, no more will be shown...\n";
        flagWarnings := false in
      kfprintf cont err_formatter fmt
    else
	begin 	
	eprintf "line %d" (line_pos ());
	eprintf ", character %d \n\t" (char_pos ());
	eprintf fmt
	end
  end
  else
    fprintf null_formatter fmt

and message fmt =
  let fmt = "@[<v 2>" ^^ fmt ^^ "@]@;@?" in
  eprintf fmt

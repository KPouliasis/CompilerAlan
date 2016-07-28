(*
 * Finalcode.ml
 *
 * Contains code necessary for the creation of MASM 8086 code for alac.
 *
 * This is free software licenced under the terms of GPL.
 * 2006 Michalis Famelis, Kostas Pouliasis
 *
 *)

open Symbol
open Types
open Identifier
open Parser
open Quad


(* Which quad we are working on. Used to print error messages *)
let cur_quad = ref 0

(* Local error handling because we have modified the functions in Module Error
 * to show the parsing pos.
 * By the way, that's the Index_out_of_bounds exception we got and could not
 * understand!!! It was trying to print out the parsing.position, when the
 * parsing was over....... ^=__=^ (ooops!)
 *)
let fatal err =
  prerr_endline ("Error while parsing quad: " ^
        (string_of_int !cur_quad));
  prerr_endline err

(* Change this to "\r\n" to write DOS format text -> apparently no need to do
 * this, ocamlrun must be taking car of this or something...
 *)  
let newline = ref "\n"

(* The output channel reference *)
let oc = ref stdout

let write_line s =
  output_string !oc (s ^ !newline);
  flush !oc

(* The current scope... It is initialized to Symbol.currentScope just because it
 * is the one eponymous scope we know. We don't use Symbol.currentScope for anything.
 * It (cursco) is supposed to be updated each time we find a UNIT quad from the
 *  value of the new function's scope.
 *)
let cursco = ref !currentScope 
(* Alternatively we could write:
 *      let cursco = Symbol.currentScope
 * but this may make it easier to explain that we only take currentScope's
 * value as an initialization
 *)
  
(* The current nesting. It is supposed to be updated for each UNIT quad. *)  
let ncurr = ref 0

(* A unique number for each function. Used in functions name and endof. It is
 * supposed to be incremented for each UNIT quad *)
let unique = ref 0

(* In which UNIT bloc we are currently parsing. Needed to write assembly for the
 * CALL quad. It is supposed to be upgraded at every UNIT quad. It is actually a
 * reference to something of type Quad._operand. *)
let current_bloc = ref (INT_CONST 0)

(* See p273. "a" is supposed to be a non-local operand entry. 
 * The for-loop is supposed to iterate ncurr-na-1 times, where:
 * ncurr is the nesting of the running routine and na is the nesting of the routine
 * that *contains* a. If a.entry_scope.sco_nesting=n the na=n-1 because a.entry_scope is
 * one scope lower than the scope of the routine that contains a.
 *)
let getAR a =
  let sco_a = a.entry_scope in
  let na = sco_a.sco_nesting -1 in
  let i = !ncurr -na -1 in
  begin
    write_line "\t mov si, word ptr [bp+4]";
    for k=1 to i do write_line "\t mov si, word ptr [si+4]" done
    (*check me, posa iterations einai afto.*)
  end

(* See p273-4. This is only called for the quad call,-,-,f 
 * In order to get the nesting of the called function, it is necessary that it
 * be passed as an argument to updateAL, even though the book does not say that
 * It accepts as an argument a Symbol.entry. *)
let updateAL f =
  let np = !ncurr in
  let sco = f.entry_scope in
  let nx = sco.sco_nesting in
  let i = np -nx -1 in
  if np < nx then
    write_line "\t push bp"
  else if np = nx then
    write_line "\t push word ptr [bp+4]"
  else (* np > nx*) begin
    write_line "\t mov si, word ptr [bp+4]";
    for k=1 to i do write_line "\t mov si, word ptr [si+4]" done;
    (*check me, posa iterations einai afto.*)
    write_line "\t push word ptr [si+4]"
  end

(* Helper routines used for load, loadAdrr and store, to make the code more
 * compact! *)  
let var_size var_info = 
  if var_info.variable_type = TYPE_byte 
  then "byte" 
  else if var_info.variable_type = TYPE_int
  then "word"
  else(
    match var_info.variable_type with
    |TYPE_array (g,_)->(
      match g with
      |TYPE_int  -> "word"
      |TYPE_byte -> "byte"
      |_->(
        fatal "var_size: array of arrays?";
        "ERROR(load)"
      )
    )
    |_-> (
      fatal "KABLAM! var_size called for crap!";
      "ERROR(load)"
    )
  )

let par_size par_info =
  if par_info.parameter_type = TYPE_byte
  then "byte"
  else if par_info.parameter_type = TYPE_int
  then "word"
  else(
    match par_info.parameter_type with
    |TYPE_array (g,_)->(
      match g with
      |TYPE_int  -> "word"
      |TYPE_byte -> "byte"
      |_->(
        fatal "par_size: array of arrays?";
        "ERROR(load)"
      )
    )
    |_-> (
      fatal "KABLAM! par_size called for crap!";
      "ERROR(load)"
    )
  )


let temp_size temp_info =
  if temp_info.temporary_type = TYPE_byte
  then "byte"
  else if temp_info.temporary_type = TYPE_int
  then "word"        
  else begin
    fatal "KABLAM! temp_size called for an... array???!!!!";
    "ERROR(load)"
  end

(* See p274-5. In this case, "a" is of type Quad._operand and reg is a string.
 *
 * This is part of the whole *ZOUMI*, that is determining weather an entry is
 * local or not. A local variable, temporary, or parameter bears with its own
 * scope, and this scope's parent. So as to consider a variable (or otherwise)
 * local, its scope_parent must be equal to the current scope. This makes more
 * sense if one remembers that if a function f declares a variable x, then if
 * f is in scope S then x is in scope "S+1", that is the parent scope of x's
 * scope is S. And of course cursco has been defined to be the scope of the
 * running function, as it was updated the last time we saw a UNIT quad.
 *)
let rec load reg a =
  let mov_reg = "\t mov " ^ reg ^ ", " in
  match a with
   INT_CONST i -> 
    write_line (mov_reg ^ (string_of_int i))
  |BYTE_CONST c ->
    let ascii_code = string_of_int (Char.code c) in
    write_line (mov_reg ^ ascii_code (*"ASCII(" ^ (String.make 1 c) ^ ")"*) )
  |OBJECT e -> begin
    let ei = e.entry_info in
    let esco = e.entry_scope in
    (*
    let es = begin match esco.sco_parent with
        |Some scp -> scp
        |None -> begin
           fatal "FC.load: there can't be an
            entry at the outer scope!!";
           !cursco
        end
    end in
    *)
    let enest = esco.sco_nesting in
    match ei with
     ENTRY_variable vi ->
       let size =  var_size vi in
       let off = 
         if vi.variable_offset > 0 then
               "+" ^ (string_of_int vi.variable_offset)
         else string_of_int vi.variable_offset
       in
       (*if es = !cursco then (* local variable *)*)
       if enest = !ncurr +1 then (* local variable *)
         write_line (mov_reg ^ size ^ " ptr[bp" ^ off ^ "]")
       else begin (* non-local variable *)
         getAR e;
         write_line (mov_reg ^ size ^ " ptr[si" ^ off ^ "]")
       end
    |ENTRY_parameter pi ->
       let size = par_size pi in
       let off = 
         if pi.parameter_offset > 0 then
              "+" ^ (string_of_int pi.parameter_offset)
         else string_of_int pi.parameter_offset
       in
       (*if es = !cursco then (* local parameter *)*)
       if enest = !ncurr +1 then (* local parameter *)
         if pi.parameter_mode = PASS_BY_VALUE then
          write_line (mov_reg ^ size ^ " ptr[bp" ^ off ^ "]")
         else (* PASS_BY_REFERENCE *) begin
          write_line ("\t mov si, word ptr[bp" ^ off ^ "]");
          write_line (mov_reg ^ size ^ " ptr[si]")
         end
       else (*non-local parameter*)
         if pi.parameter_mode = PASS_BY_VALUE then begin
           getAR e;
           write_line (mov_reg ^ size ^ " ptr[si" ^ off ^ "]")
          end
         else (* PASS_BY_REFERENCE *)begin
          getAR e;
          write_line ("\t mov si, word ptr[si" ^ off ^ "]");
          write_line (mov_reg ^ size ^ "ptr[si]")
         end
    |ENTRY_temporary ti ->
      let size = temp_size ti in
      let off = 
        if ti.temporary_offset > 0 then
             "+" ^ (string_of_int ti.temporary_offset)
        else string_of_int ti.temporary_offset
      in
      (*if es = !cursco then (*local temporary*)*)
      if enest = !ncurr +1 then (*local temporary*)
        write_line (mov_reg ^ size ^ " ptr[bp" ^ off ^ "]")
      else (* non-local temporary *) begin
        getAR e;
        write_line (mov_reg ^ size ^ " ptr[si" ^ off ^ "]")
      end
    |_ -> fatal "KAPOW! load called for entry that is not
      variable, parameter or temporary!!";
    end

  |ADDRESS w -> begin
          match w.entry_info with
          |ENTRY_temporary ti ->(
                (* load "di" (OBJECT w); (* kati san casting! *) *)
		let off = string_of_int ti.temporary_offset in
		write_line ("\t mov di, word ptr [bp" ^ off ^ "]");
                let size = temp_size ti in
                write_line ("\t mov " ^ reg ^ ", " ^ size ^ " ptr[di]")
                )
          |_-> fatal "Boom: I assumed that _operands that are Addresses are
                      always temporaries, but in this case, they are not..."          
        end
  |_ -> fatal "KABOOM! Finalcode.load called for crap!"
  
(* See p275-7. In this case,  "o" is a Quad._operand and "a" is of type Symbol.entry
 * and reg is a string. *)
let rec store reg o =
   match o with
   |OBJECT a -> begin
     let  mov = "\t mov " in
     let ai = a.entry_info in
     let asco = a.entry_scope in
     (*
     let asc = begin match asco.sco_parent with
         |Some scp -> scp
         |None -> begin
            fatal "FC.store: there can't be an
             entry at the outer scope!!";
            !cursco
         end
     end in
     *)
     let anest = asco.sco_nesting in
     match ai with
      ENTRY_variable  vi ->
        let size = var_size vi in
        let off =
          if vi.variable_offset > 0 then
           "+" ^ (string_of_int vi.variable_offset)
          else
            string_of_int vi.variable_offset
        in
        (* The book (p277) does not provide for a local variable. But
         * just in case, the code is right here. It is the same code for case
         * (b1) like local parameters (by Value) and local temps. *)
        (*if asc = !cursco then (*local variable*)*)
        if anest = !ncurr +1 then (*local variable*)
          write_line (mov ^ size ^ " ptr[bp" ^ off ^ "]," ^ reg)
        else (* non-local variable *) begin
          getAR a;
          write_line (mov ^ size ^ " ptr[si" ^ off ^ "]," ^ reg)
        end
     |ENTRY_parameter pi ->
       let size = par_size pi in
       let off =
             if pi.parameter_offset > 0 then
               "+" ^ (string_of_int pi.parameter_offset)
             else string_of_int pi.parameter_offset
       in
       (*if asc = !cursco then (* local parameter *)*)
       if anest = !ncurr +1 then (* local parameter *)
         if pi.parameter_mode = PASS_BY_VALUE then
           write_line (mov ^ size ^ " ptr[bp" ^ off ^ "]," ^ reg)
         else (* PASS_BY_REFERENCE *) 
           begin
             write_line (mov ^ "si, word ptr [bp" ^ off ^ "]");
             write_line (mov ^ size ^ " ptr[si]," ^ reg)
           end
       else (* non-local parameter *)
         if pi.parameter_mode = PASS_BY_VALUE then begin
           getAR a;
           write_line (mov ^ size ^ "ptr[si" ^ off ^ "]," ^ reg)
           end
         else (* PASS_BY_REFERENCE *) begin
           getAR a;
           write_line(mov ^ "si,word ptr [si" ^ off ^ "]");
           write_line(mov ^ size ^ " ptr[si]," ^ reg)
         end
     |ENTRY_temporary ti ->
         let size = temp_size ti in
         let off = 
           if ti.temporary_offset > 0 then 
                "+" ^ (string_of_int ti.temporary_offset)
           else string_of_int ti.temporary_offset
         in
         (*if asc = !cursco then (* local temporary *)*)
         if anest = !ncurr +1 then (* local temporary *)
           write_line(mov ^ size ^ " ptr[bp" ^ off ^ "]," ^ reg)
         else (* non-local temporary *) begin
           getAR a;
           write_line(mov ^ size ^ " ptr[si" ^ off ^ "]," ^ reg)
         end
     |_-> ignore( fatal "WAZOO! Finalcode.store called for crap!" )
      end
   |ADDRESS w ->begin
           match w.entry_info with
           |ENTRY_temporary ti ->(
                   (* load "di" (OBJECT w); (* kati san casting! *) *)
		   let off = string_of_int ti.temporary_offset in
		   write_line ("\t mov di, word ptr [bp" ^ off ^ "]");
                   let size = temp_size ti in
                   write_line ("\t mov " ^ size ^ " ptr[di]," ^ reg)           
           )
           |_-> fatal "WWWWXXX: I assumed that _operands that are Addresses are
                      always temporaries, but in this case, they are not..."          
           end
   |DOLLAR_DOLLAR -> begin
             write_line "\t mov si, word ptr [bp+6]";
             write_line ("\t mov word ptr [si], " ^ reg)
             end
   |_-> ignore( fatal "HAMAS! FC.store called for non-entry operand" )

(* The following function checks a list of strings to find a particular string.
 * If it is found it returns its position (starting in 1) in the list. If it's
 * not found it returns -1. Note that after each outside call, i has to be reset
 * to 1.
 *) 
let i = ref 1
let rec find_in_list str lst =
  match lst with
  |[] -> -1
  |head::tail ->(
    if not ((String.compare str head) = 0) then (
      incr i;
      find_in_list str tail
    )
    else(
      !i
    )           
  )
 

(* See p275-6 *)
let loadAddr2 reg a flag =
  match a with
  |OBJECT e ->(
    let ei = e.entry_info in
    let esco = e.entry_scope in
    let enest = esco.sco_nesting in
    match ei with
    |ENTRY_variable vi ->(
       let size = if flag then "word" else var_size vi in
       let off =
         if vi.variable_offset > 0 then
          "+" ^ (string_of_int vi.variable_offset)
         else
           string_of_int vi.variable_offset
       in
       if enest = !ncurr +1 then (*local variable*)
           write_line (
             "\t lea " ^ reg ^ ", " ^ size ^
             " ptr [bp" ^ off ^ "]"
           )
       else (* non-local variable *) begin
        getAR e;
	  write_line (
             "\t lea " ^ reg ^ ", " ^ size ^ 
             " ptr [si" ^ off ^ "]"
          )
       end
    )
    |ENTRY_parameter pi->(
      let size = if flag then "word" else par_size pi in
      let off =
            if pi.parameter_offset > 0 then
              "+" ^ (string_of_int pi.parameter_offset)
            else string_of_int pi.parameter_offset
      in
      if enest = !ncurr +1 then (* local parameter *)
        if pi.parameter_mode = PASS_BY_VALUE then
           write_line (
             "\t lea " ^ reg ^ ", " ^ size ^
             " ptr [bp" ^ off ^ "]"
           )
        else (* PASS_BY_REFERENCE *) 
           write_line (
             "\t mov " ^ reg ^ ", word ptr [bp" ^ off ^ "]"
           )
      else (* non-local parameter *)
        if pi.parameter_mode = PASS_BY_VALUE then begin
          getAR e;
          write_line (
             "\t lea " ^ reg ^ ", " ^ size ^ 
             " ptr [si" ^ off ^ "]"
          )
          end
        else (* PASS_BY_REFERENCE *) begin
          getAR e;
          write_line (
             "\t mov " ^ reg ^ ", word ptr [si" ^ off ^ "]"
          )
        end
      )
    |ENTRY_temporary ti->(
      let size = if flag then "word" else temp_size ti in
      let off = 
        if ti.temporary_offset > 0 then 
             "+" ^ (string_of_int ti.temporary_offset)
        else string_of_int ti.temporary_offset
      in
      if enest = !ncurr +1 then (* local temporary *)
           write_line (
             "\t lea " ^ reg ^ ", " ^ size ^
             " ptr [bp" ^ off ^ "]"
           )
      else (* non-local temporary *) begin
        getAR e;
        write_line (
           "\t lea " ^ reg ^ ", " ^ size ^ 
           " ptr [si" ^ off ^ "]"
        )
      end
    )
    |_-> fatal "loadAddr: a is not a valid entry"
    
  )
  |STRING_LITERAL s->(
    (let index = find_in_list s !string_list in
    if index >= 0 then
      let str = "@str" ^ (string_of_int index) in
      write_line( "\t lea " ^ reg ^ ", byte ptr " ^ str )
    else fatal "loadAddr: a is not a known string");
    i:=1
  )
  |ADDRESS w->(
          match w.entry_info with
          |ENTRY_temporary ti->(
	  	(*load reg (OBJECT w) (* again, a cast*) *)
		let off = string_of_int ti.temporary_offset in
		write_line ("\t mov " ^ reg ^", word ptr [bp" ^ off ^ "]");
		)
          |_-> fatal "WWWWXXX: I assumed that _operands that are Addresses are
                     always temporaries, but in this case, they are not..."          
  )
  |_-> fatal "loadAddr: a is not an entry or a string"
  
let loadAddr reg a =
  loadAddr2 reg a false
  
(* See p277. For name(f) and !unique=4 it produces the string "_f_4" 
 * See the declaration of unique above for info about it.
 * ---------------CHANGED!!!!!!!!----------------------------------
 * Instead of unique, we use f's nesting, which is unique for f throughout the
 * parsing of the quads
 * The argument f is supposed to be a Quad._operand and a function entry *)  
let name o =
  match o with
  |OBJECT f ->
    let fe = f.entry_info in
    let fsco = f.entry_scope in
    let fnest = (*fsco.sco_nesting +*) fsco.sco_id in begin
    match fe with
    ENTRY_function fi ->
      let idstr = Identifier.id_name f.entry_id in
      "_" ^ idstr ^ "_" ^ (string_of_int fnest)
    |_-> begin 
         fatal "WAJEE!!! Finalcode.name called for not a function!!";
         "dummy"
         end
    end
  |_-> begin 
       fatal "Finalcode.name called for not a sumbol.entry.
         This is a known bug, caused when alanc is called to
         parse a DOS-format file. Please, convert to UNIX-format
         and re-run alanc. Crimson Editor could do it.";
       "dummy"
       end

(* See p277-8. For endof(f) and !unique=4 it produces the string "@f_4"
 * See the declaration of unique for info about it. !unique is supposed to have
 * remained unchanged since the last call of function name. 
 * ---------------CHANGED!!!!!!!!----------------------------------
 * Instead of unique, we use f's nesting, which is unique for f throughout the
 * parsing of the quads
 * The argument f is supposed to be a Quad._operand and a function entry*)  
let endof o =
  match o with
  |OBJECT f ->
    let fe = f.entry_info in
    let fsco = f.entry_scope in
    let fnest = (*fsco.sco_nesting +*) fsco.sco_id in begin
    match fe with
    ENTRY_function fi -> 
      let idstr = id_name f.entry_id in
      "@" ^ idstr ^ "_" ^ (string_of_int fnest)
    |_->    begin 
           fatal "WAJEE!!! Finalcode.endof called for not a function!!";
           "dummy"
      end
    end
  |_->    begin 
         fatal "WAJEE!!! Finalcode.endof called for not a
         Symbol.entry!!";
         "dummy"
    end

(*        LABELS
 * This is a shitty little subject. The function label (see p278) is supposed to
 *  be called on three distinct occasions:
 *
 * 1) Before each quad code, creating an assembly label (of the form @x) that
 * corresponds to the number of the quad. For example for quad number 42 label
 * should produce the string "@42". In this case, label takes as an argument the
 * Quad.quadruple.label field, which is of type int.
 *
 * 2) Inside quad code, creating jumps to labels created from the previous
 * use of the label function. It is always used in a context similar to 
 * "jmp @42". In this case, label takes as an argument a Quad._operand that is
 * made out using the LABEL_OP constructor.
 *
 * 3) The third use involves transforming LABEL quads to assembly labels. It
 * is then possible to transform the JUMPL quad operator into a similar assembly
 * jump. We have not used LABEL quads anywhere in our code and we don't use the
 * JUMPL operator either. So this use will not be implemented at all here!
 * W_e a_r_e l_a_z_y!!! ^___^
 * 
 * For the first occasion we use a new function alltogether, named "qlabel"
 * (short for "quad label") which takes arguments of type int, as is the 
 * Quad.quadruple.label field, and uses write_line to write the label in the
 * final code file.
 *
 * For the second occasion we use a function name "label", which an argument
 * of type Quad._operand, expecting it to be using the LABEL_OP constructor.
 * It creates a string to be used along with the apropriate assembly jump command
 * in a write_line.
 *)
let qlabel ql =
  let n = string_of_int ql in
  let labl = "@" ^ n in
  write_line (labl ^ " :")
  
let label op =
  match op with
  |LABEL_OP lb -> "@" ^ (string_of_int lb)
  |_ -> begin
        fatal "WAJEE!!! FC.label called for not a LABEL_OP operand";
        "dummy"
        end
  
(* This is used to help generate assembly code for the quad CALL. It takes as an
 * argument something of type Symbol.function_info. It counts the total bytes
 * the called function uses to store its parameters. It counts 2 bytes for every
 * parameter that is an integer and is passed by value, 1 byte for every
 * parameter that is a byte and is passed by value and 2 bytes for every
 * parameter passed by reference (2 bytes are the size of memory addresses at
 * x86 architecture boxes and a call by reference is nothing but a passing of an
 * address). The output of the function is the total number as an integer.
 *)  
let count_params_size fi =
  let count = ref 0 in
  let paramlist = fi.function_paramlist in
  let add_to_count x =
    let xi = x.entry_info in
    match xi with
    |ENTRY_parameter pi ->
         let pm = pi.parameter_mode in begin
         match pm with
         |PASS_BY_REFERENCE ->
           count := !count + 2
         |PASS_BY_VALUE ->
           let pt = pi.parameter_type in begin
           match pt with
           |TYPE_int ->
          count := !count +2
           |TYPE_byte ->
          count := !count +1
           |_ -> begin
             fatal "FC.count_params_size can't
             count: I run into a parameter by value that
             is not an int or a byte. Go figure...";
             ()
           end
           end
         end
    |_-> begin
      fatal "FC.count_params_size can't count: paramlist
      contains non-parameters"
         end
  in
  ignore( List.map add_to_count paramlist );
  !count
  
(* This function updates cursco, ncurr, current_bloc and unique each time the UNIT
 * quad needs to be interpreted. Its argument is a Quad_operand. It has already been
 * checked that the operand is in fact a function entry, so no check of that
 * takes place here.
 *)
let do_unitquad_initializations x =
  match x with
  |OBJECT e ->
    let esco = e.entry_scope in
    let enest = esco.sco_nesting in begin
      cursco := esco;
      ncurr := enest;
      unique := !unique +1;
      current_bloc := x
    end
  |_ -> ignore(
    fatal "Cannot do UNIT quad initializations"
        )

(* The following two functions are used by the find_size function. They both
 * accept a Quad._operand as an argument and what they do should be obvious.
 *)
let found_local o =
  match o with
  |OBJECT e -> 
    let ei = e.entry_info in begin
    match ei with
    |ENTRY_variable _
    |ENTRY_parameter _
    |ENTRY_temporary _
    |ENTRY_function _ -> 
      let esco = e.entry_scope in
      (*let esp = match esco.sco_parent with
        |Some scp -> scp
        |None -> begin
           fatal "FC.found_local:
            tried to acess the outer scope!!";
           !cursco
        end
      in
      if esp = !cursco
      then true
      else false
      *)
      if esco.sco_nesting = !ncurr +1 then true
      else false
    |_ -> false
    end
  |_-> false

let negofs o =
  match o with
  |OBJECT e ->
    let esco = e.entry_scope in
    esco.sco_negofs
  |_-> begin
    fatal "negofs called for crap!";
    0
  end
      
(* Since a function entry does not know shit about its own subscope and we need
 * the sco_negofs from it, we have to use this so un-ocaml-like function to find
 * out the correct sco_negofs.
 *
 * We need sco_negofs at the UNIT quad. So what we do is that we check all quads
 * ahead of UNIT up to the next ENDU quad. For each one of them we check out if
 * it contains a local entry. If we find one, we rob it of its scope and get the
 *  sco_negofs. If we reach the next ENDU quad without finding any of the above,
 * that means that there are none, so the size we need is obviously zero.
 *
 * One could say that there exists an extreme case were a function declares a
 * variable and it does not use it afterwards. But even in this case it would be
 * used in a function somewhere lower at the scope hierarchy. Therefore, in this
 * extreme case there would exist at least one function declaration at the
 * immediate sub-scope, from which we can do our trick. In the extreme extreme
 * situation that the variable is declared, not used in its own scope and there
 * aren't any subroutine declarations, we can safely assume that the variable is
 * altogether not important _at_all_ and in this case the size should be zero
 * anyway.
 *
 * The function accepts a quad label of type int as an argument and returns the
 * size as a string to be used in a write_line call.
 *)
let find_size lb =
  let i = ref (lb +1) in
  let ok = ref false in
  let siz = ref 0 in
  while !ok = false do
    try
      let quad = Q.find !Quad.quadTable !i in
      let op = quad.operator in
      let x = quad.operand1 in
      let y = quad.operand2 in
      let z = quad.operand3 in
      begin
        if op = ENDU then begin
          ok:=true;
          siz:=0;             
          end
        else if (found_local x) = true then begin
          ok  := true;
          siz := negofs x
        end          
        else if (found_local y) = true then begin
          ok  := true;
          siz := negofs y
        end
        else if (found_local z) = true then begin
          ok  := true;
          siz := negofs z
        end
        else begin
          ok := false;
          i := !i +1
        end
      end
    with Not_found -> ignore(
      fatal "cannot find size for function!"
      )
  done;
  string_of_int (-(!siz))
  
  
(* Writes one quad. Actually a big match thing. *)  
let write_quad_asm quad =
  let lb = quad.label in
  let op = quad.operator in
  let x = quad.operand1 in
  let y = quad.operand2 in
  let z = quad.operand3 in
  let ax = "ax" in
  let dx = "dx" in
  let cx = "cx" in
  let al = "al" in
  let si = "si" in
  let dl = "dl" in
  qlabel lb;
  match op with
   UNIT -> begin
      do_unitquad_initializations x; 
      let size = find_size lb in
      begin
      write_line ((name x)^"\t proc near");
     
      write_line "\t push bp";
      write_line "\t mov bp,sp";
      write_line ("\t sub sp," ^ size)
      end
    end
  |ENDU -> begin
    write_line (endof x ^ " :\t mov sp,bp");
    write_line "\t pop bp";
    write_line "\t ret";
    write_line (name x ^"\t endp");
   
    end
  |PLUS -> begin
    load ax x;
    load dx y;
    write_line "\t add ax,dx";
    store ax z
    end
  |MINUS -> begin
    load ax x;
    (* Check if this is just a negation quad *)
    (match y with
    |EMPTY -> write_line "\t mov dx,0"
    |_ -> load dx y);
    write_line "\t sub ax,dx";
    store ax z
    end
  |MULT -> begin
    load ax x;
    load cx y;
    write_line "\t imul cx";
    store ax z
    end
  |DIV -> begin
    load ax x;
    write_line "\t cwd";
    load cx y;
    write_line "\t idiv cx";
    store ax z
    end
  |MOD -> begin
    load ax x;
    write_line "\t cwd";
    load cx y;
    write_line "\t idiv cx";
    store dx z
    end
  |ASSIGN -> (
    let isD = ref false in
    let type_of_operands =     (* it is supposed that type checking has already been done *)
      match z with
      |OBJECT e -> let ei = e.entry_info in begin
        match ei with
        |ENTRY_variable  vi -> vi.variable_type
        |ENTRY_parameter pi -> pi.parameter_type
        |ENTRY_temporary ti -> ti.temporary_type
        |_-> begin
          fatal "Cannot create assembly for
          assign quad: z is not an variable,
          parameter or temporary";
          TYPE_none
             end
        end
      |DOLLAR_DOLLAR -> (
        isD := true;
      	TYPE_int (* not actually, but we do need the size of an integer to store a word, as should be done in this case. *)
      )
      |ADDRESS w -> begin
              match w.entry_info with
              |ENTRY_temporary ti -> ti.temporary_type
              |_->(
                  fatal "ASSIGN: I got an ADDRESS that had no temporary with it!";
                  TYPE_none
                )
              end
      |_->   begin
        fatal "Cannot create asembly for assign
        quad: z is not an l-value";
        TYPE_none
             end
    in
    let reg = 
      if type_of_operands = TYPE_int then ax
      else al
    in 
      if (!isD = false) then (
      load reg x;
      store reg z
      ) else (
        let sizt = ( 
	      match x with
	      |OBJECT e -> let ei = e.entry_info in begin
		match ei with
		|ENTRY_variable  vi -> vi.variable_type
		|ENTRY_parameter pi -> pi.parameter_type
		|ENTRY_temporary ti -> ti.temporary_type
		|_-> begin
		  fatal "Cannot create assembly for
		  assign quad: x is not an variable,
		  parameter or temporary";
		  TYPE_none
		     end
		end
	      |INT_CONST _ -> TYPE_int
	      |BYTE_CONST _ -> TYPE_byte
	      |_->   begin
		fatal "Cannot create asembly for assign
		quad: x is not an l-value";
		TYPE_none
		     end
	)
	in 
	let siz = if (sizt = TYPE_int) then "word" else "byte" in
	let ra = if (sizt = TYPE_int) then "ax" else "al" in
	(
        load reg x;
	write_line ("\t mov si, word ptr [bp+6]");
	write_line ("\t mov "^siz^" ptr [si], " ^ ra )
	)
      )
  )
  |ARRAY -> begin
    let size = (
      match x with
      |OBJECT e -> (
        match e.entry_info with
        |ENTRY_variable v -> (
          match v.variable_type with
          |TYPE_array (g,_)->(
            if g = TYPE_int
            then 2
            else if g = TYPE_byte
            then 1
            else(
              fatal "ARRAY quad: array of
              arrays?";
              0
            )
          )
          |_->(
            fatal "ARRAY quad: x not an array";
            0
          )
        )
        |ENTRY_parameter p -> (
          match p.parameter_type with
          |TYPE_array (g,_)->(
            if g = TYPE_int
            then 2
            else if g = TYPE_byte
            then 1
            else(
              fatal "ARRAY quad: array of
              arrays?";
              0
            )
          )
          |_->(
            fatal "ARRAY quad: x not an array";
            0
          )
        )
        |ENTRY_temporary t -> (
          match t.temporary_type with
          |TYPE_array (g,_)->(
            if g = TYPE_int
            then 2
            else if g = TYPE_byte
            then 1
            else(
              fatal "ARRAY quad: array of
              arrays?";
              0
            )
          )
          |_->(
            fatal "ARRAY quad: x not an array";
            0
          )
        )
        |_ -> (
           fatal "ARRAY quad: x not a var, par or tmp";
           0
        )
      )
      |_ ->(
         fatal "ARRAY quad: x not a symbol entry";
         0
      )
    )
    in begin
      load ax y;
      write_line("\t mov cx," ^ (string_of_int size));
      write_line "\t imul cx";
      loadAddr cx x;
      write_line "\t add ax,cx";
      match z with
      |ADDRESS w -> (
        match w.entry_info with
	|ENTRY_temporary t -> (
		let off = string_of_int t.temporary_offset in
      		write_line ("\t mov word ptr [bp " ^ off ^"], ax" )
	)
	|_ -> (
		fatal "E... Gama mas kiolas..."
		)
      ) (*store ax (OBJECT w)*)
      |_-> fatal "ARRAY: where are you trying to store the address, you nut?"
      (*
      (* At this point, z contains the address of the element
       * x[i]. However, at no point do we deal with addresses.
       * So we put additional code here to load z with the
       * contents of the said address. Actually it is like
       * parsing one additional quad:
       *  :=, [z], -, z
       * This quad breaks down to two calls:
       *   load ax [z]
       *   store ax z
       * which break down to the following:  
       *)
      load "di" z;
      let siz = 
        if size = 1 then "byte"
        else if size = 2 then "word"
        else begin fatal "ARRAY quad error: size of
        array element is not 8 or 16 bits!";
        "BULLSHIT" end
      in
      write_line("\t mov ax, " ^ siz ^ " ptr[di]");
      store ax z;
      *)
    end
  end
  |EQUAL -> begin
    let type_of_operands = 
    (* it is supposed that type checking has already been done *)
      match x with
      |OBJECT e -> let ei = e.entry_info in begin
        match ei with
        |ENTRY_variable  vi -> vi.variable_type
        |ENTRY_parameter pi -> pi.parameter_type
        |ENTRY_temporary ti -> ti.temporary_type
        |_-> begin
          fatal "Cannot create assembly for
          equal quad: x is not an variable,
          parameter or temporary";
          TYPE_none
             end
        end
      |DOLLAR_DOLLAR -> TYPE_int (* not actually, but we do
        need the size of an integer to store a word, as
        should be done in this case. *)
      |ADDRESS w -> begin
              match w.entry_info with
              |ENTRY_temporary ti -> ti.temporary_type
              |_->(
                  fatal "ASSIGN: I got an ADDRESS that had no temporary with it!";
                  TYPE_none
                )
              end
      |_->   begin
        fatal "Cannot create asembly for equal
        quad: x is not an l-value";
        TYPE_none
	end
    in
    let ra = if type_of_operands = TYPE_int
      then ax
      else al
    in
    let rd = if type_of_operands = TYPE_int
      then dx
      else dl
    in
    (
    load ra x;
    load rd y;
    write_line ("\t cmp " ^ ra ^ "," ^ rd);
    write_line ("\t je " ^ (label z))
    )
  end
  |DIFFERENT -> begin
    let type_of_operands = 
    (* it is supposed that type checking has already been done *)
      match x with
      |OBJECT e -> let ei = e.entry_info in begin
        match ei with
        |ENTRY_variable  vi -> vi.variable_type
        |ENTRY_parameter pi -> pi.parameter_type
        |ENTRY_temporary ti -> ti.temporary_type
        |_-> begin
          fatal "Cannot create assembly for
          equal quad: x is not an variable,
          parameter or temporary";
          TYPE_none
             end
        end
      |DOLLAR_DOLLAR -> TYPE_int (* not actually, but we do
        need the size of an integer to store a word, as
        should be done in this case. *)
      |ADDRESS w -> begin
              match w.entry_info with
              |ENTRY_temporary ti -> ti.temporary_type
              |_->(
                  fatal "ASSIGN: I got an ADDRESS that had no temporary with it!";
                  TYPE_none
                )
              end
      |_->   begin
        fatal "Cannot create asembly for equal
        quad: x is not an l-value";
        TYPE_none
	end
    in
    let ra = if type_of_operands = TYPE_int
      then ax
      else al
    in
    let rd = if type_of_operands = TYPE_int
      then dx
      else dl
    in
    (
    load ra x;
    load rd y;
    write_line ("\t cmp " ^ ra ^ "," ^ rd);
    write_line ("\t jne " ^ (label z))
    )
  end
  |BIGGER -> begin
    let type_of_operands = 
    (* it is supposed that type checking has already been done *)
      match x with
      |OBJECT e -> let ei = e.entry_info in begin
        match ei with
        |ENTRY_variable  vi -> vi.variable_type
        |ENTRY_parameter pi -> pi.parameter_type
        |ENTRY_temporary ti -> ti.temporary_type
        |_-> begin
          fatal "Cannot create assembly for
          equal quad: x is not an variable,
          parameter or temporary";
          TYPE_none
             end
        end
      |DOLLAR_DOLLAR -> TYPE_int (* not actually, but we do
        need the size of an integer to store a word, as
        should be done in this case. *)
      |ADDRESS w -> begin
              match w.entry_info with
              |ENTRY_temporary ti -> ti.temporary_type
              |_->(
                  fatal "ASSIGN: I got an ADDRESS that had no temporary with it!";
                  TYPE_none
                )
              end
      |_->   begin
        fatal "Cannot create asembly for equal
        quad: x is not an l-value";
        TYPE_none
	end
    in
    let ra = if type_of_operands = TYPE_int
      then ax
      else al
    in
    let rd = if type_of_operands = TYPE_int
      then dx
      else dl
    in
    (
    load ra x;
    load rd y;
    write_line ("\t cmp " ^ ra ^ "," ^ rd);
    write_line ("\t jg " ^ (label z))
    )
  end
  |SMALLER -> begin
    let type_of_operands = 
    (* it is supposed that type checking has already been done *)
      match x with
      |OBJECT e -> let ei = e.entry_info in begin
        match ei with
        |ENTRY_variable  vi -> vi.variable_type
        |ENTRY_parameter pi -> pi.parameter_type
        |ENTRY_temporary ti -> ti.temporary_type
        |_-> begin
          fatal "Cannot create assembly for
          equal quad: x is not an variable,
          parameter or temporary";
          TYPE_none
             end
        end
      |DOLLAR_DOLLAR -> TYPE_int (* not actually, but we do
        need the size of an integer to store a word, as
        should be done in this case. *)
      |ADDRESS w -> begin
              match w.entry_info with
              |ENTRY_temporary ti -> ti.temporary_type
              |_->(
                  fatal "ASSIGN: I got an ADDRESS that had no temporary with it!";
                  TYPE_none
                )
              end
      |_->   begin
        fatal "Cannot create asembly for equal
        quad: x is not an l-value";
        TYPE_none
	end
    in
    let ra = if type_of_operands = TYPE_int
      then ax
      else al
    in
    let rd = if type_of_operands = TYPE_int
      then dx
      else dl
    in
    (
    load ra x;
    load rd y;
    write_line ("\t cmp " ^ ra ^ "," ^ rd);
    write_line ("\t jl " ^ (label z))
    )
  end
  |BIGGER_EQUAL -> begin
    let type_of_operands = 
    (* it is supposed that type checking has already been done *)
      match x with
      |OBJECT e -> let ei = e.entry_info in begin
        match ei with
        |ENTRY_variable  vi -> vi.variable_type
        |ENTRY_parameter pi -> pi.parameter_type
        |ENTRY_temporary ti -> ti.temporary_type
        |_-> begin
          fatal "Cannot create assembly for
          equal quad: x is not an variable,
          parameter or temporary";
          TYPE_none
             end
        end
      |DOLLAR_DOLLAR -> TYPE_int (* not actually, but we do
        need the size of an integer to store a word, as
        should be done in this case. *)
      |ADDRESS w -> begin
              match w.entry_info with
              |ENTRY_temporary ti -> ti.temporary_type
              |_->(
                  fatal "ASSIGN: I got an ADDRESS that had no temporary with it!";
                  TYPE_none
                )
              end
      |_->   begin
        fatal "Cannot create asembly for equal
        quad: x is not an l-value";
        TYPE_none
	end
    in
    let ra = if type_of_operands = TYPE_int
      then ax
      else al
    in
    let rd = if type_of_operands = TYPE_int
      then dx
      else dl
    in
    (
    load ra x;
    load rd y;
    write_line ("\t cmp " ^ ra ^ "," ^ rd);
    write_line ("\t jge " ^ (label z))
    )
  end
  |SMALLER_EQUAL -> begin
    let type_of_operands = 
    (* it is supposed that type checking has already been done *)
      match x with
      |OBJECT e -> let ei = e.entry_info in begin
        match ei with
        |ENTRY_variable  vi -> vi.variable_type
        |ENTRY_parameter pi -> pi.parameter_type
        |ENTRY_temporary ti -> ti.temporary_type
        |_-> begin
          fatal "Cannot create assembly for
          equal quad: x is not an variable,
          parameter or temporary";
          TYPE_none
             end
        end
      |DOLLAR_DOLLAR -> TYPE_int (* not actually, but we do
        need the size of an integer to store a word, as
        should be done in this case. *)
      |ADDRESS w -> begin
              match w.entry_info with
              |ENTRY_temporary ti -> ti.temporary_type
              |_->(
                  fatal "ASSIGN: I got an ADDRESS that had no temporary with it!";
                  TYPE_none
                )
              end
      |_->   begin
        fatal "Cannot create asembly for equal
        quad: x is not an l-value";
        TYPE_none
	end
    in
    let ra = if type_of_operands = TYPE_int
      then ax
      else al
    in
    let rd = if type_of_operands = TYPE_int
      then dx
      else dl
    in
    (
    load ra x;
    load rd y;
    write_line ("\t cmp " ^ ra ^ "," ^ rd);
    write_line ("\t jle " ^ (label z))
    )
  end
  |IFB -> (* not implemented by quad generator *) 
    (* but i wrote the code anyway! *)
    begin
    load "al" x;
    write_line "\t or al,al";
    write_line ("\t jnz " ^ (label z))
    end
  |LABEL -> (* not implemented by quad generator *) ()
  |JUMP -> begin
    write_line ("\t jmp " ^ (label z))
    end
  |JUMPL -> (* not implemented by quad generator *) ()
  |CALL -> begin
    match z with
    |OBJECT f ->
          let ei = f.entry_info in begin
          match ei with
          |ENTRY_function fi -> begin
          if fi.function_result = TYPE_proc then
            write_line "\t sub sp,2"
          else ();
          updateAL f;
          let name_if_lib =
            let func_sco = f.entry_scope in
              let func_nest = func_sco.sco_nesting in
              if func_nest = 0 then ("_" ^ (id_name f.entry_id)) else name z
          in                  
          write_line ("\t call near ptr " ^ name_if_lib );
          let size = count_params_size fi in
          write_line ("\t add sp," ^ (string_of_int (size+4)))
          end          
          |_ -> begin
            fatal "Cannot create assembly for quad CALL:
              z is not a function";
              ()
          end
          end
    |_ -> begin
      fatal "Cannot create assembly for quad CALL: z is
      not an entry";
      ()
          end
           

    end
  |PAR -> begin
    let byval =
      match y with
      |PASSMODE pm -> begin
        match pm with
        |V -> 0
        |R -> 1
	|RET -> 2
	end
      |_ -> begin
        fatal "Cannot create assembly code for
        quad PAR: y is not a pass mode";
        -1
            end
    in
    if (byval = 0) then
      let xtype = match x with
      |OBJECT e -> begin
        let ei = e.entry_info in
        match ei with
        |ENTRY_variable  vi -> vi.variable_type
        |ENTRY_parameter pi -> pi.parameter_type
        |ENTRY_temporary ti -> ti.temporary_type
        |_ -> begin
          fatal "Cannot create assembly for
          quad PAR: y is by value, x is an entry
          but x is not a variable, parameter or
          temporary";
          TYPE_none
              end
        end
      |INT_CONST i -> TYPE_int
      |BYTE_CONST c -> TYPE_byte
      |ADDRESS w ->(
              match w.entry_info with
              |ENTRY_temporary ti -> ti.temporary_type
              |_-> begin
                     fatal "PAR: I got an ADDRESS but it didn't have a temporary
                     with it!";
                     TYPE_none
              end
      )
      |_ -> begin
        fatal "Cannot create assembly for quad
        PAR: y is by value but x is not something that
        can be passed by value (entry, int, byte or array element)";
        TYPE_none
            end
        in begin
        if xtype = TYPE_int then begin
          load ax x;
          write_line "\t push ax"
          end 
        else if xtype = TYPE_byte then begin
          load al x;
          write_line "\t sub sp,1";
          write_line "\t mov si,sp";
          write_line "\t mov byte ptr [si],al"
          end
        else 
          ignore( fatal "Cannot create assembly for quad
          PAR: y is by value but x is not of type int or byte
          (maybe an array?)")
        end
    else if (byval = 1) (* by reference *) then
      begin
        loadAddr2 si x true;
        write_line "\t push si"
      end
    else (* byval = 2 *) (*RET*)
      begin
        let offs = (
	  match x with
	  |OBJECT o ->(
	  	match o.entry_info with
		|ENTRY_temporary ti ->(
		  ti.temporary_offset 
		)
		|_ -> (
		  fatal "par, x, ret, -: x is not a temporary!";
		  0
		)
	  )
	  |_ -> (
	    fatal "par, x, ret, -: x is not a symbol entry!";
	    0
	  )
	) in (
	  write_line ("\t lea si, word ptr [bp " ^ (string_of_int offs) ^"]");
	  write_line "\t push si"
	)	
      end
    end
  |RET_OP   -> begin
    write_line ("\t jmp " ^ (endof !current_bloc));
    end


let hex_to_decimal hex_digit=
  if (Char.code hex_digit <58 )
  then int_of_string (String.make 1 hex_digit)
  else (10+ (Char.code hex_digit-65))

  
let rec split_string2 st n part =
  try
    match st.[n] 
    with
      |'\\' -> 
   (match st.[n+1] 
    with
      |'n' -> 
         begin  
	   if part = "" then () else write_line ("\t db \'" ^ part ^ "\'");
	   write_line ("\t db " ^ (string_of_int (Char.code '\n'))); 
	   split_string2 st (succ (succ n)) "" 
         end
      |'t'->
         begin  
     if part = "" then () else write_line ("\t db \'" ^ part ^ "\'");
     write_line ("\t db " ^ (string_of_int (Char.code '\t'))); 
     split_string2 st (succ (succ n)) ""
         end
     
      |'r'->
         begin  
     if part = ""   then () else write_line ("\t db \'" ^ part ^ "\'");
     write_line ("\t db " ^ (string_of_int (Char.code '\r'))); 
          split_string2 st (succ (succ n)) ""
         end
     
      |'0'->
         begin  
     if part = "" then () else write_line ("\t db \'" ^ part ^ "\'");
          write_line ("\t db " ^ (string_of_int (Char.code '\000'))); 
          split_string2 st (succ (succ n)) ""
         end
     
      |'''->
         begin  
          if part = "" then () else write_line ("\t db \'" ^ part ^ "\'");
     write_line ("\t db " ^ (string_of_int (Char.code '''))); 
     split_string2 st (succ (succ n)) ""
         end
     
      |'"' ->
         begin  
	   if part = "" then () else write_line ("\t db \'" ^ part ^ "\'");
	   write_line ("\t db " ^ (string_of_int (Char.code '"'))); 
	   split_string2 st (succ (succ n)) ""
         end
      |'x'->
         
         begin 
	   if part = "" then () else write_line ("\t db \'" ^ part ^ "\'");
	   write_line ("\t db " ^ (string_of_int ( 16*(hex_to_decimal st.[n+2]) + (hex_to_decimal st.[n+3]))));   
				 
			(* (split_string2 
			   (Char.escaped 
			       (Char.chr 
				  (16*(hex_to_decimal st.[n+2]) + hex_to_decimal st.[n+3])))) 0 "")*) 
	   
	   split_string2 st (n+4) ""
       
         end   
     
      |_->Error.error "this should never happen: escape sequences must be exhausted";"")
     

      |a -> split_string2 st (succ n) (part ^ (String.make 1 st.[n]))
  with Invalid_argument x ->
    begin
      if part = "" 
      then ()
      else 
	write_line ("\t db \'" ^part^"\'");
      write_line "\t db 0";""
    end
      
(* Wrapper for the above, to be used below *)      
let write_strings st =
  split_string2 st 0 ""

let k = ref 0

(* Wrapper for all the above, to be used at the overall process *)      
let create_entries st = begin
  incr k;
  output_string !oc ("@str" ^ (string_of_int !k));
  flush !oc;
  write_strings st
end

(* Writes iteratively the whole quad table. *)  
let write_quadTable_asm qt =
  let i = ref 1 in
  let done_flag = ref false in
  while !done_flag = false do
    try
      let quad = Q.find qt !i in
      begin
        cur_quad := !i;
        write_quad_asm quad;
        i:= !i +1
      end
    with Not_found ->
      done_flag := true
  done
  
let main_function_name () =
  let i = ref 1 in
  let ok = ref false in
  let main = ref { 
    label=0; 
    operator=UNIT;
    operand1=EMPTY;
    operand2=EMPTY;
    operand3=EMPTY
  } in
  while !ok = false do
       try 
    let qt = !Quad.quadTable in
    let quad = Q.find qt !i in
    (if quad.operator = UNIT
    then main := quad);
    i := !i +1;
       with Not_found -> ok := true
  done;
  let main_unit_quad = !main in
  let main_func = main_unit_quad.operand1 in
  name main_func
    
(* The following two functions write the initial and final asm code that are
 * constant for every asm file we create. *)  
let write_initial_asm_code chan =
  flush !oc;
  oc:=chan;
  write_line "xseg segment public 'code'";
  write_line "\t assume cs:xseg, ds:xseg, ss:xseg";
  write_line "\t org 100h";
  write_line "main proc near";
  let program = main_function_name () in
  write_line ("\t call near ptr " ^ program); 
  write_line "\t mov ax,4C00h";
  write_line "\t int 21h";
  write_line "main endp"

let write_final_asm_code chan =
  flush !oc;
  oc:=chan;

  write_line "xseg ends";
  write_line "\t end main"

let import_lib chan=
  flush !oc;
  oc:=chan;
  
  write_line "\t extrn _writeString : proc";
  write_line "\t extrn _writeChar : proc";
  write_line "\t extrn _writeInteger : proc";
  write_line "\t extrn _readInteger : proc";
  write_line "\t extrn _readChar : proc";
  write_line "\t extrn _readString : proc";
  write_line "\t extrn _strlen : proc";
  write_line "\t extrn _strcmp : proc";
  write_line "\t extrn _strcpy : proc";
  write_line "\t extrn _strcat : proc";
  write_line "\t extrn _writeByte : proc";
  write_line "\t extrn _readByte : proc";
  write_line "\t extrn _extend : proc";
  write_line "\t extrn _shrink : proc"

(* Creates filenames such as "something.asm" from filenames such as
 * "something.alan".
 * 
 * Of course there is an implicit assumption that all alan source code files are
 * suffixed with the .alan extention.
 * 
 * For filenames that don't have the .alan extention such as "something" or
 * "something.else" it creates a filename such as "something.asm" or
 * "something.else.asm" respectively.
 * 
 * Naturally this could also be done using the Filenames module and (even
 * better) using the Str module (regular expressions stuff). But this could be
 * too much for our simple and humble cause.. *)
let asm_name oldname =
  let total = String.length oldname in
  let useful_length = total - 5 in
  try
    let useful_part = String.sub oldname 0 useful_length in
    let suffix = String.sub oldname useful_length (total -useful_length) in
    match suffix with
    ".alan"-> useful_part ^ ".asm"
    |_-> oldname ^".asm"
  with 
  Invalid_argument "String.sub"-> oldname ^ ".asm"

(* This one calls all the others. *)
let make_asm oldname =
  if ((Q.length !Quad.quadTable) = 0) 
  then 
    prerr_endline "Error: Empty Program!"
  else
  let newname = asm_name oldname in
  let out_chan = open_out newname in
    begin
      write_initial_asm_code out_chan;
      write_quadTable_asm !Quad.quadTable;
      import_lib out_chan;
      ignore( List.map create_entries (!string_list) );
      write_final_asm_code out_chan;
      close_out out_chan
    end



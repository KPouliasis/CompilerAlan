open Symbol
open Stack

module type SUBROUT=
  sig 
    type t
    type s

    val match_nth_params:int->Symbol.entry list->Quad.meta_info->bool

    val check_for_fun:Identifier.id->entry list

    val get_id:unit->Identifier.id

    val push_id:Identifier.id->unit

    val pop_id:unit ->Identifier.id

    val top_id:unit->Identifier.id

    val push_param_stuck:entry list->unit

    val pop_param_stuck:unit->entry list

    val top_param_stuck:unit->entry list

  end 

module  Subroutine= 
    struct
       
      type t=  (entry list) Stack.t
      type s=  (Identifier.id) Stack.t 

      let id_hold=Stack.create()

      let pop_id () = 	try Stack.pop id_hold
			with Empty -> 
				begin 
				Error.error "Subroutine.pop_id poped an empty stack :(";
				Identifier.id_make "dummy"
				end

      let push_id  name=Stack.push  name id_hold

      let top_id ()=	try Stack.top id_hold
			with Empty ->
				begin
				Error.error "Subroutine.top_id topped an empty stack!";
				Identifier.id_make "dummy"
				end

      let param_list_stuck=Stack.create ()

      let push_param_stuck param_list=Stack.push param_list param_list_stuck 

      let pop_param_stuck ()=	try Stack.pop param_list_stuck 
				with Empty ->
					begin
					Error.error "Subroutine.pop_param_stuck poped an empty stack :(";
					[]
					end

      let top_param_stuck ()=	try Stack.top param_list_stuck 
				with Empty ->
					begin
					Error.error "Subroutine.top_param_stuck topped an empty stack!";
					[]
					end

      let get_id ()=	try Stack.top id_hold
			with Empty ->
				begin
				Error.error "Subroutine.get_id topped an empty stack >__<";
				Identifier.id_make "dummy"
				end

      let empty=Symbol.ENTRY_none

      let check_for_fun id=
	try 
	  match (lookupEntry id LOOKUP_ALL_SCOPES true).entry_info
	  with ENTRY_function function_info->function_info.function_paramlist
	    |_->
	       begin 
		 Error.error "you can't call a non- function";[]
	       end 
	with  
	    Exit->
	      begin 
	      Error.error "attempt to call unknown function";
	      [] 
	      end 

      let param_type entry_inf=
	match entry_inf with
	  |Symbol.ENTRY_parameter param_info ->param_info.Symbol.parameter_type
	  |_->
	     begin 
		Error.fatal "This should not happen: param_type called for not a parameter";
		Types.TYPE_none
	     end 

      let param_mode entry_inf =match entry_inf with
	|Symbol.ENTRY_parameter param_info ->param_info.Symbol.parameter_mode
	|_->
	   begin
	      Error.fatal "This should not happen: param_mode called for not a parameter";
	      Symbol.PASS_BY_VALUE
	   end 

      let rec type_check type1 type2 =    (*se periptwsh arrays den me noiazei to megethos ??na chekaristei *)
	match (type1,type2) with
	  |(Types.TYPE_array (a1,a2),Types.TYPE_array (a3,a4))->type_check a1 a3
	  | (a1,a2)->(a1=a2)
      
      let  same_type  def_entry call_entry n=
      if (type_check  call_entry.Quad.meta_type (param_type def_entry)) 
      then true
      else 
	begin
	  (Error.error "wrong type in %dth parameter" n);
	  false 
	end 
	  

(*	if (call_entry.Quad.meta_type=param_type def_entry)
	then true     
	else *)
	(*  begin 
	    (Error.error "wrong type in %dth parameter " n);
	    false
	  end*)

      let check_ref def_entry call_entry n =
	if param_mode def_entry=Symbol.PASS_BY_REFERENCE
	then match call_entry.Quad.meta_place with
	  Quad.PTR(y)->true
	  |_->(Error.error "Invalid parameter in %dth argument " n) ;false  
	else true
      
      let match_nth_params n def_entry_list  call_entry=
        let n_thpar=
	  try 
	    (List.nth  def_entry_list (n-1)).Symbol.entry_info
	  with _->
	    Printf.printf "called with %d params  " n;
	    Error.error "too many arguments in call";empty 
	in
	  (same_type  n_thpar call_entry n) &&
	    (check_ref n_thpar call_entry n)
    end

module Subrout=(Subroutine:SUBROUT)

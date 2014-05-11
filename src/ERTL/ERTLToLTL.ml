
(* This module provides a translation of ERTL programs to LTL
   programs. *)

(* Adapted from Pottier's PP compiler *)

let translate_internal f (int_fun : ERTL.internal_function)
    : LTL.internal_function =

  (* Allocate a reference that will hold the control flow
     graph. Define a function that generates a statement at a fresh
     label. *)

  let graph = ref Label.Map.empty in

  let fresh_label () = Label.Gen.fresh int_fun.ERTL.f_luniverse in

  let add_graph lbl stmt = graph := Label.Map.add lbl stmt !graph in

  let generate stmt =
    let l = fresh_label () in
    add_graph l stmt ;
    l in

  (* Build the liveness analysis and the interference graph of the function. *)

  let module G = struct
    let fun_name = f
    let entry = int_fun.ERTL.f_entry
    let succs lbl = match Label.Map.find lbl int_fun.ERTL.f_graph with
      | ERTL.St_skip lbl'
      | ERTL.St_comment (_, lbl')
      | ERTL.St_cost (_, lbl')
      | ERTL.St_newframe lbl'
      | ERTL.St_delframe lbl'
      | ERTL.St_framesize (_, lbl')
      | ERTL.St_get_hdw (_, _, lbl')
      | ERTL.St_set_hdw (_, _, lbl')
      | ERTL.St_hdw_to_hdw (_, _, lbl')
      | ERTL.St_move (_, _, lbl')
      | ERTL.St_int (_, _, lbl')
      | ERTL.St_unop (_, _, _, lbl')
      | ERTL.St_binop (_, _, _, _, lbl')
      | ERTL.St_addrN (_, _, _, lbl')
      | ERTL.St_load (_, _, _, lbl')
      | ERTL.St_store (_, _, _, lbl')
      | ERTL.St_call (_, _, lbl') -> [lbl']
      | ERTL.St_cond (_, lbl1, lbl2) -> [lbl1 ; lbl2]
      | ERTL.St_tailcall _ | ERTL.St_return _ -> []
    let stmt lbl = Label.Map.find lbl int_fun.ERTL.f_graph
    let pseudo_registers = Register.Set.elements int_fun.ERTL.f_locals
    let valuation = Liveness.analyze int_fun
    let liveafter, graph = Build.build int_fun
    let uses = Uses.examine_internal int_fun
    let verbose = false
    let () =
      if verbose then
	Printf.printf "Starting hardware register allocation for %s.\n" f
  end in

  (* Instantiate the choosen register allocation algorithm. *)

  let module RegisterAllocator : Allocator.S = Choosen_allocator.Make (G) in

  (* Fetch the results of the allocation: a function that associates a physical
     location to each pseudo-register of the function, and the needed size in
     the stack. *)

  let lookup = RegisterAllocator.lookup in

  let locals = RegisterAllocator.locals in

  (* We add the additional stack size required for spilled register to the stack
     size previously required for the function: this is the final stack size
     required for the locals. *)

  let locals = locals + int_fun.ERTL.f_stacksize in

  (* The full stack size is then the size of the locals in the stack plus the
     size of the formal parameters of the function that will live in the
     stack. *)

  let stacksize =
    let formals_in_stack =
      max 0 (int_fun.ERTL.f_params -
	     (List.length Driver.TargetArch.parameters)) in
    let formals_stack_size = Driver.TargetArch.int_size * formals_in_stack in
    formals_stack_size + locals in

  let module I = ERTLToLTLI.Make (struct
    let lookup = lookup
    let generate = generate
    let fresh_label = fresh_label
    let add_graph = add_graph
    let locals = locals
    let stacksize = stacksize
  end) in

  (* Translate the instructions in the existing control flow graph.
     Pure instructions whose destination pseudo-register is dead are
     eliminated on the fly. *)

  let () =
    Label.Map.iter (fun label stmt ->
      let stmt =
	match Liveness.eliminable (G.liveafter label) stmt with
	| Some successor ->
	    LTL.St_skip successor
	| None ->
	    I.translate_statement stmt
      in
      graph := Label.Map.add label stmt !graph
    ) int_fun.ERTL.f_graph
  in

  (* Build a [LTL] function. *)

  {
    LTL.f_luniverse = int_fun.ERTL.f_luniverse;
    LTL.f_stacksize = stacksize ;
    LTL.f_entry = int_fun.ERTL.f_entry;
    LTL.f_exit = int_fun.ERTL.f_exit;
    LTL.f_graph = !graph
  }


let translate_funct (name, def) =
  let def' = match def with
    | ERTL.F_int def -> LTL.F_int (translate_internal name def)
    | ERTL.F_ext def -> LTL.F_ext def in
  (name, def')

let translate (p : ERTL.program) : LTL.program =
  { LTL.globals = p.ERTL.globals ;
    LTL.functs = List.map translate_funct p.ERTL.functs ;
    LTL.main = p.ERTL.main }

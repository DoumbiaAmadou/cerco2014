
(** This module translates a [LIN] program into a [ASM] program. *)


let error_prefix = "LIN to ASM"
let error s = Error.global_error error_prefix s


let translate_statement = function
  | LIN.St_skip lbl -> [Arch.IGoto lbl]
  | LIN.St_label lbl -> [Arch.ILabel lbl]
  | LIN.St_comment s -> [Arch.IComment (false, s)]
  | LIN.St_cost lbl -> [Arch.ICost lbl]
  | LIN.St_int (r, i) -> [Arch.IConst (r, i)]
  | LIN.St_addr (addr, x) -> [Arch.ILoadAddr (addr, x)]
  | LIN.St_unop (unop, destr, srcr) ->
    [Arch.IUnOp (unop, destr, srcr)]
  | LIN.St_binop (binop, destr, srcr1, srcr2) ->
    [Arch.IBinOp (binop, destr, srcr1, srcr2)]
  | LIN.St_load (size, destr, addr) ->
    [Arch.ILoad (Arch.data_size_of_byte_size size, destr, addr)]
  | LIN.St_store (size, addr, destr) ->
    [Arch.IStore (Arch.data_size_of_byte_size size, addr, destr)]
  | LIN.St_call f -> [Arch.ICall f]
  | LIN.St_tailcall f -> [Arch.IGotor f]
  | LIN.St_cond (r, lbl) -> [Arch.IBranch (r, lbl)]
  | LIN.St_return -> [Arch.IReturn]

let translate_code code =
  List.flatten (List.map translate_statement code)


(** [translate_fun_def fundef] returns the translation of the body of the [LIN]
    function [fundef] into a list of assembly instructions. *)

let translate_fun_def ((id, def) : (string * LIN.function_def))
    : Driver.TargetArch.register Arch.generic_instructions =
  match def with
    | LIN.F_int code -> (Arch.ILabel id) :: (translate_code code)
    | LIN.F_ext ext -> []


(** [code functs] returns the inlined assembly instructions obtained by
    concatenating the translations of the body of the [LIN] functions
    [functs] *)

let code (functs : (string * LIN.function_def) list)
    : Driver.TargetArch.register Arch.generic_instructions =
  assert false (* TODO M1 *)


let externals functs =
  let f externals (_, def) = match def with
    | LIN.F_int _ -> externals
    | LIN.F_ext def -> def :: externals in
  List.fold_left f [] functs


(* Translating programs. *)

let translate p =
  { Arch.globals = p.LIN.globals ;
    Arch.externals = externals p.LIN.functs ;
    Arch.main = p.LIN.main ;
    Arch.code = code p.LIN.functs }

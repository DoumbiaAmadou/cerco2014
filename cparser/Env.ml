(* *********************************************************************)
(*                                                                     *)
(*              The Compcert verified compiler                         *)
(*                                                                     *)
(*          Xavier Leroy, INRIA Paris-Rocquencourt                     *)
(*                                                                     *)
(*  Copyright Institut National de Recherche en Informatique et en     *)
(*  Automatique.  All rights reserved.  This file is distributed       *)
(*  under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 2 of the License, or  *)
(*  (at your option) any later version.  This file is also distributed *)
(*  under the terms of the INRIA Non-Commercial License Agreement.     *)
(*                                                                     *)
(* *********************************************************************)

(* Typing environment *)

open C

type error =
  | Unbound_identifier of string
  | Unbound_tag of string * string
  | Tag_mismatch of string * string * string
  | Unbound_typedef of string
  | No_member of string * string * string

exception Error of error

(* Maps over ident, accessible both by name or by name + stamp *)

module StringMap = Map.Make(String)

module IdentMap = struct
  type 'a t = (ident * 'a) list StringMap.t
  let empty : 'a t = StringMap.empty

  (* Search by name and return topmost binding *)
  let lookup s m =
    match StringMap.find s m with
    | id_data :: _ -> id_data
    | [] -> assert false

  (* Search by identifier and return associated binding *)
  let find id m =
    let rec lookup_in = function
    | [] -> raise Not_found
    | (id', data) :: rem ->
         if id'.stamp = id.stamp then data else lookup_in rem in
    lookup_in (StringMap.find id.name m)

  (* Insert by identifier *)
  let add id data m =
    let l = try StringMap.find id.name m with Not_found -> [] in
    StringMap.add id.name ((id, data) :: l) m
end

let gensym = ref 0

let fresh_ident s = incr gensym; { name = s; stamp = !gensym }

(* Infos associated with structs or unions *)

type composite_info = {
  ci_kind: struct_or_union;
  ci_members: field list;               (* members, in order *)
  ci_alignof: int option;               (* alignment; None if incomplete *)
  ci_sizeof: int option;                (* size; None if incomplete *)
}

(* Infos associated with an ordinary identifier *)

type ident_info =
  | II_ident of storage * typ
  | II_enum of int64                    (* value of the enum *)

(* Infos associated with a typedef *)

type typedef_info = typ

(* Environments *)

type t = {
  env_scope: int;
  env_ident: ident_info IdentMap.t;
  env_tag: composite_info IdentMap.t;
  env_typedef: typedef_info IdentMap.t
}

let empty = {
  env_scope = 0;
  env_ident = IdentMap.empty;
  env_tag = IdentMap.empty;
  env_typedef = IdentMap.empty
}

(* Enter a new scope. *)

let new_scope env =
  { env with env_scope = !gensym + 1 }

let in_current_scope env id = id.stamp >= env.env_scope

(* Looking up things by source name *)

let lookup_ident env s =
  try
    IdentMap.lookup s env.env_ident
  with Not_found ->
    raise(Error(Unbound_identifier s))

let lookup_tag env s =
  try
    IdentMap.lookup s env.env_tag
  with Not_found ->
    raise(Error(Unbound_tag(s, "tag")))

let lookup_struct env s =
  try
    let (id, ci as res) = IdentMap.lookup s env.env_tag in
    if ci.ci_kind <> Struct then
      raise(Error(Tag_mismatch(s, "struct", "union")));
    res
  with Not_found ->
    raise(Error(Unbound_tag(s, "struct")))
 
let lookup_union env s =
  try
    let (id, ci as res) = IdentMap.lookup s env.env_tag in
    if ci.ci_kind <> Union then
      raise(Error(Tag_mismatch(s, "union", "struct")));
    res
  with Not_found ->
    raise(Error(Unbound_tag(s, "union")))
 
let lookup_composite env s =
  try Some (IdentMap.lookup s env.env_tag)
  with Not_found -> None

let lookup_typedef env s =
  try
    IdentMap.lookup s env.env_typedef
  with Not_found ->
    raise(Error(Unbound_typedef s))

(* Checking if a source name is bound *)

let ident_is_bound env s = StringMap.mem s env.env_ident

(* Finding things by translated identifier *)

let find_ident env id =
  try IdentMap.find id env.env_ident
  with Not_found ->
    raise(Error(Unbound_identifier(id.name)))

let find_tag env id =
  try IdentMap.find id env.env_tag
  with Not_found ->
    raise(Error(Unbound_tag(id.name, "tag")))

let find_struct env id =
  try
    let ci = IdentMap.find id env.env_tag in
    if ci.ci_kind <> Struct then
      raise(Error(Tag_mismatch(id.name, "struct", "union")));
    ci
  with Not_found ->
    raise(Error(Unbound_tag(id.name, "struct")))

let find_union env id =
  try
    let ci = IdentMap.find id env.env_tag in
    if ci.ci_kind <> Union then
      raise(Error(Tag_mismatch(id.name, "union", "struct")));
    ci
  with Not_found ->
    raise(Error(Unbound_tag(id.name, "union")))
 
let find_member ci m =
  List.find (fun f -> f.fld_name = m) ci

let find_struct_member env (id, m) =
  try
    let ci = find_struct env id in
    find_member ci.ci_members m
  with Not_found ->
    raise(Error(No_member(id.name, "struct", m)))

let find_union_member env (id, m) =
  try
    let ci = find_union env id in
    find_member ci.ci_members m
  with Not_found ->
    raise(Error(No_member(id.name, "union", m)))

let find_typedef env id =
  try
    IdentMap.find id env.env_typedef
  with Not_found ->
    raise(Error(Unbound_typedef(id.name)))

(* Inserting things by source name, with generation of a translated name *)

let enter_ident env s sto ty =
  let id = fresh_ident s in
  (id,
   { env with env_ident = IdentMap.add id (II_ident(sto, ty)) env.env_ident })

let enter_composite env s ci =
  let id = fresh_ident s in
  (id, { env with env_tag = IdentMap.add id ci env.env_tag })

let enter_enum_item env s v =
  let id = fresh_ident s in
  (id, { env with env_ident = IdentMap.add id (II_enum v) env.env_ident })

let enter_typedef env s info =
  let id = fresh_ident s in
  (id, { env with env_typedef = IdentMap.add id info env.env_typedef })

(* Inserting things by translated name *)

let add_ident env id sto ty =
  { env with env_ident = IdentMap.add id (II_ident(sto, ty)) env.env_ident }

let add_composite env id ci =
  { env with env_tag = IdentMap.add id ci env.env_tag }

let add_typedef env id info =
  { env with env_typedef = IdentMap.add id info env.env_typedef }

(* Error reporting *)

open Printf

let error_message = function
  | Unbound_identifier name -> 
      sprintf "Unbound identifier '%s'" name
  | Unbound_tag(name, kind) ->
      sprintf "Unbound %s '%s'" kind name
  | Tag_mismatch(name, expected, actual) ->
      sprintf "'%s' was declared as a %s but is used as a %s" 
              name actual expected
  | Unbound_typedef name ->
      sprintf "Unbound typedef '%s'" name
  | No_member(compname, compkind, memname) ->
      sprintf "%s '%s' has no member named '%s'"
              compkind compname memname

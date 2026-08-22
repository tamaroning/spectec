open Il

type arity =
  | Exact of int
  | Star
  | StarPlus of int

type region =
  | Local of string
  | Glob of string
  | GlobStar
  | Mem of string
  | Table
  | Elem
  | Data
  | Struct
  | Array
  | Exn

type summary = {
  name : string;
  pops : arity;
  pushes : arity;
  reads : region list;
  writes : region list;
  may_trap : bool;
  may_throw : bool;
  may_diverge : bool;
  may_nondet : bool;
}

val string_of_arity : arity -> string
val string_of_regions : region list -> string

(** Extract per-variant effect summaries for loop/jump-free instructions. *)
val extract : Ast.script -> summary list

module Atom : sig
  type src =
    | Pop of int
    | Local of string
    | Glob of string

  type write = {
    region : [`Local | `Glob];
    idx : string;
    src : src;
  }

  type t = {
    name : string;
    pop : int;
    push : src list;
    writes : write list;
  }
end

(** Closed stack/local/global transformers for the gapped-rule alphabet. *)
val extract_atoms : Ast.script -> Atom.t list

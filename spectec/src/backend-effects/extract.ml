(* Effect-summary extraction for loop/jump-free Wasm instructions.
   Sources: binary grammar (variants), Instr_ok (arity),
   Step/Step_pure/Step_read (footprints), numeric defs (per-op mayTrap). *)

open Util.Source
open Xl
open Il.Ast

type arity =
  | Exact of int
  | Star
  | StarPlus of int

type region =
  | Local of string
  | Glob of string
  | GlobStar
  | Mem of string
  | Table of string
  | Elem of string
  | Data of string
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

let string_of_arity = function
  | Exact n -> string_of_int n
  | Star -> "*"
  | StarPlus n -> "*+" ^ string_of_int n

let region_rank = function
  | Local _ -> 0
  | Glob _ -> 1
  | GlobStar -> 2
  | Mem _ -> 3
  | Table _ -> 4
  | Elem _ -> 5
  | Data _ -> 6
  | Struct -> 7
  | Array -> 8
  | Exn -> 9

let string_of_region = function
  | Local x -> "local(" ^ x ^ ")"
  | Glob x -> "glob(" ^ x ^ ")"
  | GlobStar -> "glob(*)"
  | Mem x -> "mem(" ^ x ^ ")"
  | Table x -> "table(" ^ x ^ ")"
  | Elem x -> "elem(" ^ x ^ ")"
  | Data x -> "data(" ^ x ^ ")"
  | Struct -> "struct"
  | Array -> "array"
  | Exn -> "exn"

let cmp_region a b =
  let c = compare (region_rank a) (region_rank b) in
  if c <> 0 then c else
  match a, b with
  | Mem x, Mem y | Table x, Table y | Elem x, Elem y | Data x, Data y
  | Local x, Local y | Glob x, Glob y -> String.compare x y
  | _ -> 0

let string_of_regions rs =
  List.sort_uniq cmp_region rs |> List.map string_of_region |> String.concat ";"

(* ===== Helpers ===== *)

let rec typ_name t =
  match t.it with
  | VarT (x, _) -> x.it
  | IterT (t', _) -> typ_name t'
  | _ -> ""

let rec strip e =
  match e.it with
  | SubE (e', _, _) | TheE e' | CvtE (e', _, _) | UncaseE (e', _)
  | ProjE (e', _) ->
    strip e'
  | _ -> e

let mixop_args e =
  match (strip e).it with
  | TupE es -> es
  | _ -> [e]

let mixop_head op =
  match Mixop.head op with
  | Some atom -> Atom.to_string atom
  | None -> ""

let rec case_head e =
  match (strip e).it with
  | CaseE (op, _) -> mixop_head op
  | IterE (e', _) -> case_head e'
  | _ -> ""

let rec iter_exp f e =
  f e;
  match e.it with
  | VarE _ | BoolE _ | NumE _ | TextE _ -> ()
  | UnE (_, _, e1) | ProjE (e1, _) | UncaseE (e1, _) | TheE e1
  | LiftE e1 | LenE e1 | CvtE (e1, _, _) | SubE (e1, _, _) ->
    iter_exp f e1
  | BinE (_, _, e1, e2) | CmpE (_, _, e1, e2) | CompE (e1, e2)
  | CatE (e1, e2) | IdxE (e1, e2) | MemE (e1, e2) ->
    iter_exp f e1; iter_exp f e2
  | SliceE (e1, e2, e3) | IfE (e1, e2, e3) ->
    iter_exp f e1; iter_exp f e2; iter_exp f e3
  | TupE es | ListE es -> List.iter (iter_exp f) es
  | OptE eo -> Option.iter (iter_exp f) eo
  | CaseE (_, e1) -> iter_exp f e1
  | StrE fs -> List.iter (fun (_, e1) -> iter_exp f e1) fs
  | DotE (e1, _) -> iter_exp f e1
  | UpdE (e1, p, e2) | ExtE (e1, p, e2) ->
    iter_exp f e1; iter_path f p; iter_exp f e2
  | CallE (_, args) -> List.iter (iter_arg f) args
  | IterE (e1, (it, ies)) ->
    iter_exp f e1;
    (match it with ListN (n, _) -> iter_exp f n | _ -> ());
    List.iter (fun (_, e') -> iter_exp f e') ies

and iter_path f p =
  match p.it with
  | RootP -> ()
  | IdxP (p', e) -> iter_path f p'; iter_exp f e
  | SliceP (p', e1, e2) -> iter_path f p'; iter_exp f e1; iter_exp f e2
  | DotP (p', _) -> iter_path f p'

and iter_arg f = function
  | {it = ExpA e; _} -> iter_exp f e
  | _ -> ()

let rec iter_prem f p =
  match p.it with
  | IfPr e -> iter_exp f e
  | LetPr (_, e1, e2) -> iter_exp f e1; iter_exp f e2
  | RulePr (_, args, _, e) -> List.iter (iter_arg f) args; iter_exp f e
  | IterPr (p', _) | NegPr p' -> iter_prem f p'
  | ElsePr -> ()

(* Never-fall-through / handler / block structure: no effect-summary row.
   THROW / THROW_REF jump like BR (uncaught → invoke abort, or to a handler);
   mayThrow on summaries arises only from CALL* in the straight-line fragment. *)
let control_heads = [
  "BLOCK"; "LOOP"; "IF"; "ELSE"; "END";
  "BR"; "BR_IF"; "BR_TABLE";
  "BR_ON_NULL"; "BR_ON_NON_NULL"; "BR_ON_CAST"; "BR_ON_CAST_FAIL";
  "RETURN"; "RETURN_CALL"; "RETURN_CALL_INDIRECT"; "RETURN_CALL_REF";
  "TRY"; "TRY_TABLE"; "CATCH"; "CATCH_ALL";
  "THROW"; "THROW_REF"; "RETHROW";
]

let is_control head =
  List.mem head control_heads || String.starts_with ~prefix:"BR_" head

let call_heads = ["CALL"; "CALL_INDIRECT"; "CALL_REF"]
let is_call head = List.mem head call_heads

let value_heads = [
  "CONST"; "VCONST";
  "REF.NULL"; "REF.FUNC"; "REF.FUNC_ADDR"; "REF.HOST_ADDR";
  "REF.NULL_ADDR"; "REF.I31_NUM"; "REF.EXTERN"; "REF.I31";
]

let rec is_value e =
  let e = strip e in
  match e.it with
  | VarE x ->
    let n = x.it in
    n = "val" || n = "ref" || String.starts_with ~prefix:"val" n
  | CaseE (op, _) -> List.mem (mixop_head op) value_heads
  | IterE (e', _) -> is_value e'
  | OptE (Some e') -> is_value e'
  | _ ->
    let tn = typ_name e.note in
    tn = "val" || tn = "ref" || tn = "num" || tn = "vec"

let is_trap e = case_head e = "TRAP"

let rec flatten_seq e =
  match (strip e).it with
  | ListE es -> List.concat_map flatten_seq es
  | CatE (e1, e2) -> flatten_seq e1 @ flatten_seq e2
  | IterE (e', _) when is_value e' -> []
  | OptE None -> []
  | OptE (Some e') -> flatten_seq e'
  | _ -> [e]

let rec last_instr es =
  match List.rev es with
  | [] -> None
  | e :: rest when is_value e || is_trap e -> last_instr (List.rev rest)
  | e :: _ -> Some e

(* ===== Regions ===== *)

module RSet = Set.Make (struct
  type t = region
  let compare = cmp_region
end)

let region_list s = RSet.elements s

let write_defs = [
  "with_local"; "with_global"; "with_mem"; "with_meminst";
  "with_table"; "with_tableinst";
  "with_elem"; "with_data"; "with_struct"; "with_array";
  "add_structinst"; "add_arrayinst"; "add_exninst";
]
let read_defs = [
  "local"; "global"; "mem"; "table"; "meminst"; "tableinst";
  "elem"; "data"; "structinst"; "arrayinst"; "eleminst"; "datainst"; "exninst";
]

let idx_name e =
  match (strip e).it with
  | VarE x -> x.it
  | NumE (`Nat z) -> Z.to_string z
  | NumE (`Int z) -> Z.to_string z
  | _ -> "*"

let exp_of_arg = function
  | {it = ExpA e; _} -> Some e
  | _ -> None

let nth_exp args i =
  match List.nth_opt args i with
  | Some a -> (match exp_of_arg a with Some e -> idx_name e | None -> "*")
  | None -> "*"

let region_of_call id args ~write =
  match id.it, write with
  | "local", false | "with_local", true -> Some (Local (nth_exp args 1))
  | "global", false | "with_global", true -> Some (Glob (nth_exp args 1))
  | "mem", false | "meminst", false
  | "with_mem", true | "with_meminst", true -> Some (Mem (nth_exp args 1))
  | "table", false | "tableinst", false
  | "with_table", true | "with_tableinst", true -> Some (Table (nth_exp args 1))
  | "elem", false | "eleminst", false | "with_elem", true -> Some (Elem (nth_exp args 1))
  | "data", false | "datainst", false | "with_data", true -> Some (Data (nth_exp args 1))
  | "structinst", false | "with_struct", true | "add_structinst", true -> Some Struct
  | "arrayinst", false | "with_array", true | "add_arrayinst", true -> Some Array
  | "exninst", false | "add_exninst", true -> Some Exn
  | _ -> None

(* Append helpers also read the frontier length; count them as reads too. *)
let collect_regions e =
  let reads = ref RSet.empty in
  let writes = ref RSet.empty in
  iter_exp (fun e ->
    match e.it with
    | CallE (id, args) ->
      if List.mem id.it write_defs then begin
        Option.iter (fun r -> writes := RSet.add r !writes)
          (region_of_call id args ~write:true);
        (match id.it with
         | "add_structinst" | "add_arrayinst" | "add_exninst" ->
           Option.iter (fun r -> reads := RSet.add r !reads)
             (region_of_call id args ~write:true)
         | _ -> ())
      end else if List.mem id.it read_defs then
        Option.iter (fun r -> reads := RSet.add r !reads)
          (region_of_call id args ~write:false)
    | _ -> ()
  ) e;
  !reads, !writes

let collect_regions_prems prems =
  List.fold_left (fun (r, w) p ->
    let acc_r = ref r in
    let acc_w = ref w in
    iter_prem (fun e ->
      let rr, ww = collect_regions e in
      acc_r := RSet.union !acc_r rr;
      acc_w := RSet.union !acc_w ww
    ) p;
    !acc_r, !acc_w
  ) (RSet.empty, RSet.empty) prems

let collect_effect_flags e =
  let may_throw = ref false in
  iter_exp (fun e ->
    match e.it with
    | CallE (id, _) when id.it = "add_exninst" -> may_throw := true
    | _ -> ()
  ) e;
  !may_throw

let collect_flags_prems prems =
  let tr = ref false in
  List.iter (fun p ->
    iter_prem (fun e -> tr := !tr || collect_effect_flags e) p
  ) prems;
  !tr

(* ===== Unification ===== *)

module SMap = Map.Make (String)

let rec unify subst p c =
  let p = strip p and c = strip c in
  match p.it, c.it with
  | _, _ when Il.Eq.eq_exp p c -> Some subst
  | VarE x, _ ->
    (match SMap.find_opt x.it subst with
    | None -> Some (SMap.add x.it c subst)
    | Some e -> unify subst e c)
  | _, VarE _ -> Some subst
  | CaseE (op1, a1), CaseE (op2, a2)
    when Mixop.eq op1 op2
      || (mixop_head op1 = mixop_head op2
          && mixop_head op1 <> ""
          && Mixop.arity op1 = Mixop.arity op2) ->
    unify subst a1 a2
  | TupE ps, TupE cs when List.length ps = List.length cs ->
    unify_list subst ps cs
  | ListE ps, ListE cs when List.length ps = List.length cs ->
    unify_list subst ps cs
  | OptE None, OptE None -> Some subst
  | OptE (Some p'), OptE (Some c') -> unify subst p' c'
  | IterE (p', _), IterE (c', _) -> unify subst p' c'
  | IterE (p', _), _ -> unify subst p' c
  | _, IterE (c', _) -> unify subst p c'
  | CatE (p1, p2), CatE (c1, c2) ->
    Option.bind (unify subst p1 c1) (fun s -> unify s p2 c2)
  | LiftE p', LiftE c' -> unify subst p' c'
  | LiftE p', _ -> unify subst p' c
  | _, LiftE c' -> unify subst p c'
  | CallE (f1, a1), CallE (f2, a2) when f1.it = f2.it ->
    unify_args subst a1 a2
  | _ -> None

and unify_list subst ps cs =
  List.fold_left2 (fun acc p c ->
    Option.bind acc (fun s -> unify s p c)
  ) (Some subst) ps cs

and unify_args subst a1 a2 =
  if List.length a1 <> List.length a2 then None else
  List.fold_left2 (fun acc x y ->
    Option.bind acc (fun s ->
      match x.it, y.it with
      | ExpA e1, ExpA e2 -> unify s e1 e2
      | _ -> Some s)
  ) (Some subst) a1 a2

let rec subst_exp subst e =
  let e = strip e in
  match e.it with
  | VarE x -> (match SMap.find_opt x.it subst with Some e' -> e' | None -> e)
  | CallE (f, args) ->
    {e with it = CallE (f, List.map (subst_arg subst) args)}
  | CaseE (op, a) -> {e with it = CaseE (op, subst_exp subst a)}
  | TupE es -> {e with it = TupE (List.map (subst_exp subst) es)}
  | ListE es -> {e with it = ListE (List.map (subst_exp subst) es)}
  | OptE eo -> {e with it = OptE (Option.map (subst_exp subst) eo)}
  | LiftE e1 -> {e with it = LiftE (subst_exp subst e1)}
  | _ -> e

and subst_arg subst a =
  match a.it with
  | ExpA e -> {a with it = ExpA (subst_exp subst e)}
  | _ -> a

(* ===== Numeric emptiness ===== *)

let is_eps e =
  match (strip e).it with
  | ListE [] | OptE None -> true
  | _ -> false

let rec return_is_opt t =
  match t.it with
  | IterT (_, Opt) -> true
  | IterT (t', (List | List1)) -> return_is_opt t'
  | _ -> false

let rec contains_sub s sub i =
  let n = String.length s and m = String.length sub in
  i + m <= n && (String.sub s i m = sub || contains_sub s sub (i + 1))

(* Option-typed builtins may fail, except saturating conversions. *)
let builtin_may_fail id =
  not (contains_sub (String.lowercase_ascii id.it) "sat" 0)

let rec can_be_empty env visited e =
  let e = strip e in
  match e.it with
  | ListE [] | OptE None -> true
  | ListE _ | OptE (Some _) -> false
  | LiftE e1 -> can_be_none env visited e1
  | IfE (_, e1, e2) -> can_be_empty env visited e1 || can_be_empty env visited e2
  | CallE (f, args) when f.it = "list_" ->
    (match List.nth_opt args 1 with
    | Some {it = ExpA e1; _} ->
      can_be_none env visited e1 || can_be_empty env visited e1
      || call_may_be_empty env visited f args
    | _ -> call_may_be_empty env visited f args)
  | CallE (f, args) -> call_may_be_empty env visited f args
  | _ -> false

and can_be_none env visited e =
  let e = strip e in
  match e.it with
  | OptE None -> true
  | OptE (Some _) -> false
  | CallE (f, args) -> call_may_be_none env visited f args
  | IfE (_, e1, e2) -> can_be_none env visited e1 || can_be_none env visited e2
  | _ -> false

and call_may_be_empty env visited f args =
  if List.mem f.it visited then false else
  match Il.Env.find_opt_def env f with
  | None -> false
  | Some (_ps, typ, clauses) ->
    if clauses = [] then return_is_opt typ && builtin_may_fail f else
    List.exists (fun cl ->
      match cl.it with
      | DefD (_, cargs, body, _) ->
        (match unify_args SMap.empty cargs args with
        | None -> false
        | Some subst ->
          let body = subst_exp subst body in
          is_eps body || can_be_empty env (f.it :: visited) body)
    ) clauses

and call_may_be_none env visited f args =
  if List.mem f.it visited then false else
  match Il.Env.find_opt_def env f with
  | None -> false
  | Some (_ps, typ, clauses) ->
    if clauses = [] then return_is_opt typ && builtin_may_fail f else
    List.exists (fun cl ->
      match cl.it with
      | DefD (_, cargs, body, _) ->
        (match unify_args SMap.empty cargs args with
        | None -> false
        | Some subst ->
          let body = subst_exp subst body in
          is_eps body || can_be_none env (f.it :: visited) body)
    ) clauses

let rec prem_allows_empty env subst p =
  match p.it with
  | IfPr e ->
    (match (strip e).it with
    | CmpE (`EqOp, _, e1, e2) when is_eps e2 ->
      can_be_empty env [] (subst_exp subst e1)
    | CmpE (`EqOp, _, e1, e2) when is_eps e1 ->
      can_be_empty env [] (subst_exp subst e2)
    | _ -> true)
  | ElsePr | RulePr _ | LetPr _ -> true
  | IterPr (p', _) | NegPr p' -> prem_allows_empty env subst p'

let trap_rule_fires env subst prems =
  List.for_all (prem_allows_empty env subst) prems

(* ===== Arity ===== *)

type count = {n : int; star : bool}

let add_count a b = {n = a.n + b.n; star = a.star || b.star}

let rec count_vals e =
  let e = strip e in
  match e.it with
  | ListE es ->
    List.fold_left (fun acc e -> add_count acc (count_vals e))
      {n = 0; star = false} es
  | CatE (e1, e2) -> add_count (count_vals e1) (count_vals e2)
  | IterE (_, ((List | List1), _)) -> {n = 0; star = true}
  | IterE (_, (ListN (n, _), _)) ->
    (match (strip n).it with
    | NumE (`Nat z) -> {n = Z.to_int z; star = false}
    | _ -> {n = 0; star = true})
  | IterE (e', (Opt, _)) -> count_vals e'
  | CaseE (_, a) ->
    (match (strip a).it with
    | TupE [] -> {n = 1; star = false}
    | ListE _ | CatE _ | IterE _ | CaseE _ | TupE _ -> count_vals a
    | _ -> {n = 1; star = false})
  | OptE None -> {n = 0; star = false}
  | OptE (Some e') -> count_vals e'
  | TupE [] -> {n = 0; star = false}
  | TupE es ->
    List.fold_left (fun acc e -> add_count acc (count_vals e))
      {n = 0; star = false} es
  | LiftE e' -> count_vals e'
  | _ -> {n = 1; star = false}

let arity_of_count c =
  if c.star && c.n = 0 then Star
  else if c.star then StarPlus c.n
  else Exact c.n

let mixop_atoms op = Mixop.flatten op |> List.flatten

let is_arrow_mixop op =
  List.exists (fun a -> match a.it with Atom.Arrow | Atom.ArrowSub -> true | _ -> false)
    (mixop_atoms op)

let rec functype_arity e =
  let e = strip e in
  match e.it with
  | CaseE (op, a) when is_arrow_mixop op ->
    (match mixop_args a with
    | [ins; outs] | [ins; _; outs] -> Some (count_vals ins, count_vals outs)
    | _ -> None)
  | CaseE (_, a) -> functype_arity a
  | TupE ([ins; outs] | [ins; _; outs]) ->
    Some (count_vals ins, count_vals outs)
  | _ -> None

let instr_ok_sides e =
  match mixop_args e with
  | _c :: instr :: typ :: _ -> Some (instr, typ)
  | [instr; typ] -> Some (instr, typ)
  | _ -> None

(* ===== Naming ===== *)

let rec atom_tokens e acc =
  let e = strip e in
  match e.it with
  | CaseE (op, a) ->
    let acc =
      match Mixop.head op with
      | Some at ->
        let s = Atom.to_string at in
        if s = "_" then acc else s :: acc
      | None -> acc
    in
    atom_tokens a acc
  | TupE es | ListE es ->
    List.fold_left (fun acc e -> atom_tokens e acc) acc es
  | OptE (Some e') | LiftE e' | IterE (e', _) -> atom_tokens e' acc
  | NumE (`Nat z) | NumE (`Int z) -> Z.to_string z :: acc
  | VarE x ->
    let n = x.it in
    if List.mem n ["x"; "y"; "ao"; "memarg"; "i"; "c"; "bt"; "ts"; "t"; "n"; "p"; "_"]
    then acc else n :: acc
  | CallE (f, args)
    when f.it = "sz" || String.ends_with ~suffix:"sz" f.it ->
    List.fold_left (fun acc a ->
      match exp_of_arg a with Some e -> atom_tokens e acc | None -> acc
    ) acc args
  | _ -> acc

let type_token = function
  | "I32" | "i32" -> Some "i32"
  | "I64" | "i64" -> Some "i64"
  | "F32" | "f32" -> Some "f32"
  | "F64" | "f64" -> Some "f64"
  | "V128" | "v128" -> Some "v128"
  | "I8" | "i8" -> Some "i8"
  | "I16" | "i16" -> Some "i16"
  | _ -> None

let sx_token = function
  | "S" | "s" -> Some "s"
  | "U" | "u" -> Some "u"
  | _ -> None

let drop_ctor = function
  | h :: rest when List.mem h [
      "BINOP"; "UNOP"; "TESTOP"; "RELOP"; "CVTOP"; "CONST";
      "LOAD"; "STORE"; "VLOAD"; "VSTORE"; "VCONST";
      "VBINOP"; "VUNOP"; "VTESTOP"; "VRELOP"; "VVUNOP"; "VVBINOP";
      "VVTERNOP"; "VVTESTOP"; "VSHIFTOP"; "VBITMASK"; "VSWIZZLE";
      "VSHUFFLE"; "VSPLAT"; "VEXTRACT_LANE"; "VREPLACE_LANE";
      "VEXTUNOP"; "VEXTBINOP"; "VNARROW"; "VCVTOP";
      "VLOAD_LANE"; "VSTORE_LANE";
    ] -> rest
  | toks -> toks

let rec join_op toks =
  match toks with
  | [] -> ""
  | t :: rest ->
    (match type_token t, sx_token t with
    | Some ty, _ ->
      ty ^ (if rest = [] then "" else "." ^ join_op rest)
    | _, Some sx ->
      let op = join_op rest in
      if op = "" then sx else op ^ "_" ^ sx
    | None, None ->
      let t = String.lowercase_ascii t in
      if rest = [] then t
      else
        let rest' = join_op rest in
        if String.for_all (function '0'..'9' -> true | _ -> false) rest'
        then t ^ rest'
        else t ^ "_" ^ rest')

let shape_name toks =
  match toks with
  | t1 :: n :: rest
    when type_token t1 <> None
      && String.for_all (function '0'..'9' -> true | _ -> false) n ->
    Option.get (type_token t1) ^ "x" ^ n ^
      (if rest = [] then "" else "." ^ join_op rest)
  | _ -> join_op toks

let instr_name e =
  let head = case_head e in
  match head with
  | "" -> "unknown"
  | "BINOP" | "UNOP" | "TESTOP" | "RELOP" | "CONST" ->
    let toks = List.rev (atom_tokens e []) |> drop_ctor in
    (match toks with
    | ty :: op when type_token ty <> None ->
      Option.get (type_token ty) ^ "." ^
        (if op = [] then String.lowercase_ascii head else join_op op)
    | _ -> String.lowercase_ascii head)
  | "CVTOP" ->
    let toks = List.rev (atom_tokens e []) |> drop_ctor in
    (match toks with
    | dest :: src :: rest
      when type_token dest <> None && type_token src <> None ->
      let dest = Option.get (type_token dest) in
      let src = Option.get (type_token src) in
      let sx, op =
        match List.rev rest with
        | s :: op when sx_token s <> None ->
          Some (Option.get (sx_token s)), List.rev op
        | _ -> None, rest
      in
      dest ^ "." ^ join_op op ^ "_" ^ src ^
        (match sx with None -> "" | Some s -> "_" ^ s)
    | dest :: rest when type_token dest <> None ->
      Option.get (type_token dest) ^ "." ^ join_op rest
    | _ -> "cvtop")
  | "LOAD" | "STORE" | "VLOAD" | "VSTORE" | "VLOAD_LANE" | "VSTORE_LANE" ->
    let toks = List.rev (atom_tokens e []) |> drop_ctor in
    let kind =
      match head with
      | "VLOAD" -> "load"
      | "VSTORE" -> "store"
      | "VLOAD_LANE" -> "load_lane"
      | "VSTORE_LANE" -> "store_lane"
      | _ -> String.lowercase_ascii head
    in
    (match toks with
    | ty :: rest when type_token ty <> None ->
      let ty = Option.get (type_token ty) in
      (match rest with
      | [] -> ty ^ "." ^ kind
      | n :: sx when String.for_all (function '0'..'9' -> true | _ -> false) n ->
        let sx = join_op sx in
        ty ^ "." ^ kind ^ n ^ (if sx = "" then "" else "_" ^ sx)
      | _ -> ty ^ "." ^ kind ^ "_" ^ join_op rest)
    | _ -> kind)
  | "VBINOP" | "VUNOP" | "VTESTOP" | "VRELOP" | "VSHIFTOP" | "VSPLAT"
  | "VBITMASK" | "VSWIZZLE" | "VSHUFFLE" | "VEXTRACT_LANE" | "VREPLACE_LANE"
  | "VEXTUNOP" | "VEXTBINOP" | "VNARROW" | "VCVTOP" | "VVUNOP" | "VVBINOP"
  | "VVTERNOP" | "VVTESTOP" ->
    List.rev (atom_tokens e []) |> drop_ctor |> shape_name
  | "VCONST" -> "v128.const"
  | _ -> String.lowercase_ascii head

(* ===== Inventory ===== *)

let rec fold_defs f acc ds =
  List.fold_left (fun acc d ->
    match d.it with
    | RecD ds' -> fold_defs f acc ds'
    | _ -> f acc d
  ) acc ds

let is_binstr_gram x =
  let n = x.it in
  n = "Binstr" || String.starts_with ~prefix:"Binstr" n

let collect_variants script =
  fold_defs (fun acc d ->
    match d.it with
    | GramD (x, _, _, prods) when is_binstr_gram x ->
      List.fold_left (fun acc p ->
        match p.it with
        | ProdD (_, _, e, _) ->
          let h = case_head e in
          if h <> "" && not (is_control h) then e :: acc else acc
      ) acc prods
    | _ -> acc
  ) [] script
  |> List.rev

(* ===== Step rules ===== *)

let find_rel env name =
  Il.Env.find_opt_rel env (name $ no_region)

let rel_rules env name =
  match find_rel env name with
  | Some (_, _, _, rules) -> rules
  | None -> []

let rule_expr rule =
  match rule.it with RuleD (_, _, _, e, prems) -> e, prems

let step_sides e =
  match mixop_args e with
  | [lhs; rhs] -> Some (lhs, rhs)
  | _ -> None

let config_instrs e =
  let e = strip e in
  match e.it with
  | CaseE (_, a) ->
    let args = mixop_args a in
    if List.length args >= 2 then Some (Util.Lib.List.last args) else Some e
  | ListE _ | CatE _ -> Some e
  | _ -> Some e

let seq_of_side e =
  match config_instrs e with
  | Some s -> flatten_seq s
  | None -> flatten_seq e

let rhs_has_trap e = List.exists is_trap (seq_of_side e)

let residual_heads e =
  seq_of_side e
  |> List.filter (fun e -> not (is_value e) && not (is_trap e))
  |> List.filter_map (fun e ->
      let h = case_head e in
      if h = "" then None else Some h)

type ctor_info = {
  reads : RSet.t;
  writes : RSet.t;
  trap : bool;
  may_throw : bool;
  may_nondet : bool;
  residuals : string list;
}

let empty_info = {
  reads = RSet.empty; writes = RSet.empty; trap = false;
  may_throw = false; may_nondet = false; residuals = [];
}

let union_info a b = {
  reads = RSet.union a.reads b.reads;
  writes = RSet.union a.writes b.writes;
  trap = a.trap || b.trap;
  may_throw = a.may_throw || b.may_throw;
  may_nondet = a.may_nondet || b.may_nondet;
  residuals = List.sort_uniq String.compare (a.residuals @ b.residuals);
}

type step_row = {
  head : string;
  focus : exp;
  lhs_sig : string;
  info : ctor_info;
  prems : prem list;
}

(* Coarse LHS shape: value constructors before the focused instruction.
   Distinguishes REF.NULL_ADDR vs REF.STRUCT_ADDR so trap/success pairs
   are not treated as overlapping; grow succeed/fail share a signature. *)
let lhs_value_sig lhs focus =
  let prefix =
    match List.rev (seq_of_side lhs) with
    | f :: pref when case_head f = case_head focus -> List.rev pref
    | _ -> []
  in
  prefix
  |> List.map (fun e ->
      match case_head e with
      | "" -> "_"
      | h -> h)
  |> String.concat "/"

let analyze_rule rule =
  let e, prems = rule_expr rule in
  match step_sides e with
  | None -> None
  | Some (lhs, rhs) ->
    match last_instr (seq_of_side lhs) with
    | None -> None
    | Some focus ->
      let h = case_head focus in
      if h = "" then None else
      let rr, rw = collect_regions rhs in
      let pr, pw = collect_regions_prems prems in
      let lr, lw = collect_regions lhs in
      let fr = collect_effect_flags rhs in
      let prf = collect_flags_prems prems in
      Some {
        head = h;
        focus;
        lhs_sig = lhs_value_sig lhs focus;
        info = {
          reads = RSet.union pr (RSet.union rr lr);
          writes = RSet.union pw (RSet.union rw lw);
          trap = rhs_has_trap rhs;
          may_throw = fr || prf;
          may_nondet = false;
          residuals = residual_heads rhs;
        };
        prems;
      }

let index_step_rules env =
  List.concat_map (rel_rules env) ["Step_pure"; "Step_read"; "Step"]
  |> List.filter_map analyze_rule

let call_info = {
  reads = RSet.of_list [
    GlobStar; Mem "*"; Table "*"; Elem "*"; Data "*"; Struct; Array; Exn;
  ];
  writes = RSet.of_list [
    GlobStar; Mem "*"; Table "*"; Elem "*"; Data "*"; Struct; Array; Exn;
  ];
  trap = true;
  may_throw = true;
  may_nondet = true;
  residuals = [];
}

let ctor_fixpoint (indexed : step_row list) =
  let tbl = Hashtbl.create 64 in
  List.iter (fun row ->
    let prev =
      match Hashtbl.find_opt tbl row.head with Some i -> i | None -> empty_info
    in
    Hashtbl.replace tbl row.head (union_info prev row.info)
  ) indexed;
  List.iter (fun h -> Hashtbl.replace tbl h call_info) call_heads;
  let changed = ref true in
  while !changed do
    changed := false;
    Hashtbl.iter (fun h info ->
      if is_call h then () else
      let acc =
        List.fold_left (fun acc r ->
          match Hashtbl.find_opt tbl r with
          | None -> acc
          | Some i -> union_info acc i
        ) info info.residuals
      in
      if not (RSet.equal acc.reads info.reads
              && RSet.equal acc.writes info.writes
              && acc.trap = info.trap
              && acc.may_throw = info.may_throw
              && acc.may_nondet = info.may_nondet) then begin
        Hashtbl.replace tbl h {acc with residuals = info.residuals};
        changed := true
      end
    ) tbl
  done;
  tbl

let match_focus variant row =
  if case_head variant <> row.head then None else
  unify SMap.empty row.focus variant

let variant_trap env variant indexed =
  List.exists (fun row ->
    match match_focus variant row with
    | None -> false
    | Some subst ->
      row.info.trap && trap_rule_fires env subst row.prems
  ) indexed

let lookup_arity env variant =
  let rec find = function
    | [] -> None
    | rule :: rest ->
      let e, _ = rule_expr rule in
      match instr_ok_sides e with
      | None -> find rest
      | Some (instr, typ) ->
        (match unify SMap.empty instr variant with
        | None -> find rest
        | Some _ ->
          (match functype_arity typ with
          | Some (p, q) -> Some (arity_of_count p, arity_of_count q)
          | None -> find rest))
  in
  match find (rel_rules env "Instr_ok") with
  | Some a -> a
  | None ->
    if case_head variant = "UNREACHABLE" then (Exact 0, Exact 0)
    else (Star, Star)

let union_regions a b = List.sort_uniq cmp_region (a @ b)

let merge_arity a b =
  match a, b with
  | Exact n, Exact m when n = m -> Exact n
  | Exact n, Exact _ -> Exact n
  | Exact n, _ | _, Exact n -> Exact n
  | StarPlus n, StarPlus m when n = m -> StarPlus n
  | StarPlus n, _ | _, StarPlus n -> StarPlus n
  | Star, Star -> Star

let merge_summary a b = {
  name = a.name;
  pops = merge_arity a.pops b.pops;
  pushes = merge_arity a.pushes b.pushes;
  reads = union_regions a.reads b.reads;
  writes = union_regions a.writes b.writes;
  may_trap = a.may_trap || b.may_trap;
  may_throw = a.may_throw || b.may_throw;
  may_diverge = a.may_diverge || b.may_diverge;
  may_nondet = a.may_nondet || b.may_nondet;
}

(* Deterministic-profile default: relaxed SIMD stays ¬mayNondet, by name
   (call is the dual hardcode that forces true). *)
let is_relaxed_variant e =
  List.exists (fun t ->
    contains_sub (String.uppercase_ascii t) "RELAXED" 0
  ) (atom_tokens e [])

let is_relaxed_name name =
  contains_sub (String.lowercase_ascii name) "relaxed" 0

(* Grow-style overlap: a rule with no side conditions alongside one with
   some (fail vs succeed). Broader subset would mark every oob/success
   pair nondeterministic. `otherwise` vs `if` stays exclusive. *)
let prem_keys prems =
  List.filter_map (fun p ->
    match p.it with
    | ElsePr -> None
    | _ -> Some (Il.Print.string_of_prem p)
  ) prems
  |> List.sort_uniq String.compare

let has_else_prem prems =
  List.exists (fun p -> match p.it with ElsePr -> true | _ -> false) prems

let positively_overlaps prems1 prems2 =
  let s1 = prem_keys prems1 in
  let s2 = prem_keys prems2 in
  if (has_else_prem prems1 && s2 <> []) || (has_else_prem prems2 && s1 <> []) then
    false
  else
    (s1 = [] && s2 <> []) || (s2 = [] && s1 <> [])

let head_has_overlapping_rules (indexed : step_row list) head =
  let rs = List.filter (fun r -> r.head = head) indexed in
  let rec pairs = function
    | [] | [_] -> false
    | a :: rest ->
      List.exists (fun b ->
        a.lhs_sig = b.lhs_sig && positively_overlaps a.prems b.prems
      ) rest
      || pairs rest
  in
  pairs rs

let extract script =
  let env = Il.Env.env_of_script script in
  let variants = collect_variants script in
  let indexed = index_step_rules env in
  let ctor_tbl = ctor_fixpoint indexed in
  let rows =
    List.filter_map (fun variant ->
      let head = case_head variant in
      if head = "" || is_control head then None else
      let info =
        if is_call head then call_info
        else match Hashtbl.find_opt ctor_tbl head with
        | Some i -> i
        | None -> empty_info
      in
      let pops, pushes = lookup_arity env variant in
      let pops, pushes =
        match head with
        | "UNREACHABLE" -> (Exact 0, Exact 0)
        | "CALL" | "CALL_REF" -> (Star, Star)
        | "CALL_INDIRECT" -> (StarPlus 1, Star)
        | _ -> (pops, pushes)
      in
      let name = instr_name variant in
      let relaxed = is_relaxed_variant variant || is_relaxed_name name in
      Some {
        name;
        pops; pushes;
        reads = region_list info.reads;
        writes = region_list info.writes;
        may_trap =
          if is_call head then true else variant_trap env variant indexed;
        may_throw =
          info.may_throw || is_call head;
        may_diverge = is_call head;
        may_nondet =
          (* Name hardcodes: calls ⊤; relaxed SIMD ⊥ (deterministic profile). *)
          if relaxed then false
          else
            info.may_nondet || is_call head
            || head_has_overlapping_rules indexed head;
      }
    ) variants
  in
  let order = ref [] in
  let tbl = Hashtbl.create 256 in
  List.iter (fun s ->
    match Hashtbl.find_opt tbl s.name with
    | None -> Hashtbl.add tbl s.name s; order := s.name :: !order
    | Some prev -> Hashtbl.replace tbl s.name (merge_summary prev s)
  ) rows;
  List.rev !order |> List.map (Hashtbl.find tbl)


(* ===== Closed atom semantics for drop, locals, nop, and globals ===== *)

module Atom = struct
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

  let eq_src a b =
    match a, b with
    | Pop i, Pop j -> i = j
    | Local x, Local y | Glob x, Glob y -> x = y
    | _ -> false

  let eq_write a b =
    a.region = b.region && a.idx = b.idx && eq_src a.src b.src
end

let atom_heads = [
  "DROP"; "LOCAL.GET"; "LOCAL.SET"; "LOCAL.TEE";
  "NOP"; "GLOBAL.GET"; "GLOBAL.SET";
]

let atom_name head = String.lowercase_ascii head

let atom_order = List.map atom_name atom_heads

exception Atom_error of string

let atom_fail msg = raise (Atom_error msg)

let find_name_index n xs =
  let rec go i = function
    | [] -> None
    | y :: ys -> if y = n then Some i else go (i + 1) ys
  in go 0 xs

let rec first_var e =
  match (strip e).it with
  | VarE x -> Some x.it
  | TupE es | ListE es -> List.find_map first_var es
  | CaseE (_, a) -> first_var a
  | OptE (Some e') | IterE (e', _) | LiftE e' -> first_var e'
  | CallE (_, args) ->
    List.find_map (function {it = ExpA e'; _} -> first_var e' | _ -> None) args
  | _ -> None

let rec unwrap_val e =
  let e = strip e in
  match e.it with
  | OptE (Some e') | LiftE e' | DotE (e', _) -> unwrap_val e'
  | _ -> e

let config_state e =
  let e = strip e in
  match e.it with
  | CaseE (_, a) ->
    (match mixop_args a with
    | st :: _ :: _ -> Some st
    | _ -> None)
  | _ -> None

type raw_src =
  | RName of string
  | RLocal of string
  | RGlob of string

let raw_src_of e =
  let e = unwrap_val e in
  match e.it with
  | VarE x -> RName x.it
  | CallE (id, args) when id.it = "local" -> RLocal (nth_exp args 1)
  | CallE (id, args) when id.it = "global" -> RGlob (nth_exp args 1)
  | _ -> atom_fail "atom-sem: value is not a stack or local/global source"

let rec prem_src_binds acc p =
  match p.it with
  | IfPr e -> eq_src_binds acc e
  | LetPr (_, e1, e2) -> eq_pair_binds acc e1 e2
  | IterPr (p', _) | NegPr p' -> prem_src_binds acc p'
  | RulePr _ | ElsePr -> acc

and eq_src_binds acc e =
  match (strip e).it with
  | CmpE (`EqOp, _, a, b) -> eq_pair_binds acc a b
  | _ -> acc

and eq_pair_binds acc a b =
  let bind map name src = SMap.add name src map in
  let a, b = unwrap_val a, unwrap_val b in
  match a.it, b.it with
  | VarE x, CallE (id, args) when id.it = "local" ->
    bind acc x.it (RLocal (nth_exp args 1))
  | CallE (id, args), VarE x when id.it = "local" ->
    bind acc x.it (RLocal (nth_exp args 1))
  | VarE x, CallE (id, args) when id.it = "global" ->
    bind acc x.it (RGlob (nth_exp args 1))
  | CallE (id, args), VarE x when id.it = "global" ->
    bind acc x.it (RGlob (nth_exp args 1))
  | _ -> acc

let resolve_raw pop_names prem_binds = function
  | RLocal x -> Atom.Local x
  | RGlob x -> Atom.Glob x
  | RName n ->
    match find_name_index n pop_names with
    | Some i -> Atom.Pop i
    | None ->
      match SMap.find_opt n prem_binds with
      | Some (RLocal x) -> Atom.Local x
      | Some (RGlob x) -> Atom.Glob x
      | Some (RName _) | None ->
        atom_fail ("atom-sem: unbound value " ^ n)

let decode_writes pop_names prem_binds st =
  match st with
  | None -> []
  | Some e ->
    (match (strip e).it with
    | VarE _ -> []
    | CallE (id, args) when id.it = "with_local" || id.it = "with_global" ->
      let region = if id.it = "with_local" then `Local else `Glob in
      let idx = nth_exp args 1 in
      let src =
        match List.nth_opt args 2 with
        | Some a ->
          (match exp_of_arg a with
          | Some v -> resolve_raw pop_names prem_binds (raw_src_of v)
          | None -> atom_fail ("atom-sem: $" ^ id.it ^ " missing value"))
        | None -> atom_fail ("atom-sem: $" ^ id.it ^ " missing value")
      in
      [{ Atom.region; idx; src }]
    | CallE (id, _) ->
      atom_fail ("atom-sem: unsupported state update $" ^ id.it)
    | _ -> atom_fail "atom-sem: unsupported state expression")

type raw_atom = {
  head : string;
  pop : int;
  push : Atom.src list;
  writes : Atom.write list;
  residual : (string * string option) option;
}

let take_last n xs =
  let rec split k ys =
    if k <= 0 then [], ys else
    match ys with
    | [] -> atom_fail "atom-sem: residual stack underflow"
    | y :: rest ->
      let pre, post = split (k - 1) rest in
      y :: pre, post
  in
  let k = List.length xs - n in
  if k < 0 then atom_fail "atom-sem: residual stack underflow";
  split k xs

let remap_src env = function
  | Atom.Pop i ->
    (match List.nth_opt env i with
    | Some s -> s
    | None -> atom_fail "atom-sem: residual pop out of range")
  | (Atom.Local _ | Atom.Glob _) as s -> s

let remap_write env (w : Atom.write) =
  { w with Atom.src = remap_src env w.src }

let rec close_raw depth tbl (r : raw_atom) =
  if depth > 8 then atom_fail ("atom-sem: residual cycle at " ^ r.head);
  match r.residual with
  | None -> r
  | Some (rh, _) ->
    let inner =
      match Hashtbl.find_opt tbl rh with
      | Some i -> i
      | None -> atom_fail ("atom-sem: residual " ^ rh ^ " is outside the atom fragment")
    in
    let inner = close_raw (depth + 1) tbl inner in
    let prefix, popped = take_last inner.pop r.push in
    {
      head = r.head;
      pop = r.pop;
      push = prefix @ List.map (remap_src popped) inner.push;
      writes = r.writes @ List.map (remap_write popped) inner.writes;
      residual = inner.residual;
    }

let parse_raw_atom rule =
  let e, prems = rule_expr rule in
  match step_sides e with
  | None -> None
  | Some (lhs, rhs) ->
    match last_instr (seq_of_side lhs) with
    | None -> None
    | Some focus ->
      let h = case_head focus in
      if not (List.mem h atom_heads) then None else
      if rhs_has_trap rhs then
        atom_fail ("atom-sem: " ^ h ^ " reduces to TRAP")
      else
      let is_atom_value e =
        is_value e ||
        (match (unwrap_val e).it with
        | CallE (id, _) when id.it = "local" || id.it = "global" -> true
        | _ -> false)
      in
      let lhs_vals = seq_of_side lhs |> List.filter is_atom_value in
      let pop_names =
        List.map (fun v ->
          match (unwrap_val v).it with
          | VarE x -> x.it
          | _ -> atom_fail ("atom-sem: " ^ h ^ " operand is not a variable")
        ) lhs_vals
      in
      let prem_binds = List.fold_left prem_src_binds SMap.empty prems in
      let rhs_seq = seq_of_side rhs in
      if List.exists is_trap rhs_seq then
        atom_fail ("atom-sem: " ^ h ^ " reduces to TRAP")
      else
      let rhs_vals, rhs_rest = List.partition is_atom_value rhs_seq in
      let residual =
        match rhs_rest with
        | [] -> None
        | [r] ->
          let rh = case_head r in
          if rh = "" then
            atom_fail ("atom-sem: " ^ h ^ " has a non-instruction residual");
          Some (rh, first_var r)
        | _ -> atom_fail ("atom-sem: " ^ h ^ " has multiple residuals")
      in
      let push =
        List.map (fun v -> resolve_raw pop_names prem_binds (raw_src_of v)) rhs_vals
      in
      let writes = decode_writes pop_names prem_binds (config_state rhs) in
      Some {
        head = h;
        pop = List.length pop_names;
        push;
        writes;
        residual;
      }

let raw_to_atom (r : raw_atom) : Atom.t =
  if r.residual <> None then
    atom_fail ("atom-sem: " ^ r.head ^ " did not close into the fragment");
  {
    Atom.name = atom_name r.head;
    pop = r.pop;
    push = r.push;
    writes = r.writes;
  }

let extract_atoms script =
  try
    let env = Il.Env.env_of_script script in
    let parsed =
      List.concat_map (rel_rules env) ["Step_pure"; "Step_read"; "Step"]
      |> List.filter_map parse_raw_atom
    in
    let tbl = Hashtbl.create 16 in
    List.iter (fun (r : raw_atom) ->
      match Hashtbl.find_opt tbl r.head with
      | None -> Hashtbl.replace tbl r.head r
      | Some prev ->
        if prev.pop <> r.pop
           || prev.residual <> r.residual
           || List.length prev.push <> List.length r.push
           || List.length prev.writes <> List.length r.writes
           || not (List.for_all2 Atom.eq_src prev.push r.push)
           || not (List.for_all2 Atom.eq_write prev.writes r.writes)
        then
          atom_fail ("atom-sem: conflicting rules for " ^ r.head)
    ) parsed;
    let closed = Hashtbl.create 16 in
    List.iter (fun h ->
      match Hashtbl.find_opt tbl h with
      | None -> atom_fail ("atom-sem: missing reduction for " ^ h)
      | Some r ->
        let c = close_raw 0 tbl r in
        Hashtbl.replace closed h (raw_to_atom c)
    ) atom_heads;
    List.map (fun name ->
      let head = List.find (fun h -> atom_name h = name) atom_heads in
      Hashtbl.find closed head
    ) atom_order
  with Atom_error msg ->
    Util.Error.error no_region "atom-sem" msg


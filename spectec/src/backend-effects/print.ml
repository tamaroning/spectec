open Extract

let csv_escape s =
  if String.contains s ',' || String.contains s '"' || String.contains s '\n'
  then
    let buf = Buffer.create (String.length s + 8) in
    Buffer.add_char buf '"';
    String.iter (fun c ->
      if c = '"' then Buffer.add_string buf "\"\"" else Buffer.add_char buf c
    ) s;
    Buffer.add_char buf '"';
    Buffer.contents buf
  else s

let row s =
  String.concat "," [
    csv_escape s.name;
    string_of_arity s.pops;
    string_of_arity s.pushes;
    csv_escape (string_of_regions s.reads);
    csv_escape (string_of_regions s.writes);
    string_of_bool s.may_trap;
    string_of_bool s.may_diverge;
  ]

let header = "instruction,pops,pushes,reads,writes,mayTrap,mayDiverge"

let to_string summaries =
  String.concat "\n" (header :: List.map row summaries) ^ "\n"

let output oc summaries =
  output_string oc (to_string summaries)

let json_string s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  String.iter (function
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.add_char buf '"';
  Buffer.contents buf

let json_src = function
  | Atom.Pop i -> Printf.sprintf {|{"pop": %d}|} i
  | Atom.Local x ->
    Printf.sprintf {|{"region": "local", "idx": %s}|} (json_string x)
  | Atom.Glob x ->
    Printf.sprintf {|{"region": "glob", "idx": %s}|} (json_string x)

let json_write (w : Atom.write) =
  let region = match w.region with `Local -> "local" | `Glob -> "glob" in
  Printf.sprintf {|{"region": %s, "idx": %s, "from": %s}|}
    (json_string region) (json_string w.idx) (json_src w.src)

let json_list f xs =
  match xs with
  | [] -> "[]"
  | _ ->
    "[" ^ String.concat ", " (List.map f xs) ^ "]"

let json_atom (a : Atom.t) =
  String.concat "\n" [
    "    {";
    "      \"instruction\": " ^ json_string a.name ^ ",";
    "      \"pop\": " ^ string_of_int a.pop ^ ",";
    "      \"push\": " ^ json_list json_src a.push ^ ",";
    "      \"writes\": " ^ json_list json_write a.writes;
    "    }";
  ]

let atoms_to_string atoms =
  let body =
    match atoms with
    | [] -> ""
    | _ -> "\n" ^ String.concat ",\n" (List.map json_atom atoms) ^ "\n  "
  in
  "{\n  \"atoms\": [" ^ body ^ "]\n}\n"

let output_atoms oc atoms =
  output_string oc (atoms_to_string atoms)

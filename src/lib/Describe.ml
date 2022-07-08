let describe_workspace () =
  let tmp = Filename.temp_file "describe" ".sexp" in
  ignore (Sys.command
            (Filename.quote_command ~stdout:tmp
               "dune" ["describe"; "workspace"; "--with-deps"]));
  Fun.protect ~finally:(fun () -> Sys.remove tmp) @@ fun () ->
  Sexplib.Sexp.load_sexp tmp

module Utils = struct

let ( let+ ) m f = List.map f m
let ( let* ) m f = List.concat_map f m

open Sexplib.Sexp

let select_all names entries =
  List.filter_map (function
    | List [Atom s; List data] when List.mem s names -> Some data
    | _ -> None
  ) entries

let single = function
  | [v] -> v
  | _ -> invalid_arg "single"

let select_single names entries =
  List.filter_map (function
    | List [Atom s; v] when List.mem s names -> Some v
    | _ -> None
  ) entries
  |> single

let list = function
  | List li -> li
  | _ -> invalid_arg "list"

let atom = function
  | Atom s -> s
  | _ -> invalid_arg "atom"

let list_of conv sexp =
  list sexp
  |> List.map conv

let extract_strings sexp_list =
  List.filter_map (function
    | Atom s -> Some s
    | List _ -> None
  ) sexp_list

let read_entries entries =
  let entries = list entries in
  let* (entry_name, entry) =
    begin
      let+ exec = select_all ["executables"] entries in
      (select_single ["names"] exec |> list_of atom |> single, exec)
    end @ begin
      let+ lib = select_all ["library"] entries in
      (select_single ["name"] lib |> atom, lib)
    end
  in
  let+ module_ = select_single ["modules"] entry |> list_of list in
  let name = select_single ["name"] module_ |> atom in
  let impl = select_single ["impl"] module_ |> list_of atom |> single in
  let deps =
    let deps = select_single ["module_deps"] module_ |> list in
    (select_single ["for_intf"] deps |> list_of atom)
    @
    (select_single ["for_impl"] deps |> list_of atom)
  in
  Printf.eprintf "%s: %s\n%!" name (String.concat ", " deps);
  let qualify name = entry_name ^ "." ^ name in
  Dep_graph.(Node.{
    name = Name.Lib (qualify name);
    kind = Node.Lib;
    deps = List.map qualify deps;
    loc = Loc.{ path = impl; index = 0 };
  })
end

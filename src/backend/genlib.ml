(* open Core_unix *)
(* open Graph.Persistent.Graph *)

open Mybuild_parser.Ast

type t = myfile

(* let embox_root = "/home/cy/Desktop/ocaml-rep/embox" *)
(* TODO: make pure *)
let path_embox_build_base_gen =
  "/home/cy/Desktop/ocaml-rep/caravan/test/backend_test_local/caravan_build/base/gen"
;;

(** mkdir -p (Core_unix.mkdir_p replacement). *)
let mkdir_p (dir : string) : unit =
  let acc = ref "" in
  Base.List.iter (Base.String.split ~on:'/' dir) ~f:(fun p ->
    if not (Base.String.is_empty p) then (
      acc :=
        if Base.String.is_empty !acc then "/" ^ p else !acc ^ "/" ^ p;
      (try
         Unix.mkdir !acc 0o755
       with Unix.Unix_error (Unix.EEXIST, _, _) -> ())))
;;

let gen_package package =
  let _ = package in
  let dir = Base.String.concat ~sep:"/" (path_embox_build_base_gen :: package) in
  (* let _ = Printf.printf "%s\n" cat in *)
  let () = mkdir_p dir in
  Result.Ok ()
;;

let gen_imports imports =
  let _ = imports in
  Result.Ok ()
;;

let gen_entities entities =
  let _ = entities in
  Result.Ok ()
;;

let gen (trees : t list) : unit =
  Base.List.iter
    ~f:(fun (package, imports, entities) ->
      let unwrap_package =
        match package with
        | Some x -> x
        | None -> []
      in
      match gen_package unwrap_package, gen_imports imports, gen_entities entities with
      | Result.Ok (), Result.Ok (), Result.Ok () -> ()
      | _ -> ())
    trees
;;

(* Dump (module_type_fqn, super_type_fqn) pairs for hierarchy validation. *)
open Base

let () =
  let argv = Sys.get_argv () in
  let root =
    if Array.length argv > 1 then Array.get argv 1
    else "/home/kakadu/asp/embox-rukaml"
  in
  let t = Registry.load ~root in
  let pairs =
    List.map (Registry.all_module_types t) ~f:(fun mt ->
      let sup =
        match mt.mt_super with
        | None -> ""
        | Some s -> Model.fqn_to_string s.mt_fqn
      in
      (Model.fqn_to_string mt.mt_fqn, sup))
  in
  let pairs = List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b) pairs in
  List.iter pairs ~f:(fun (fqn, sup) -> Stdlib.Format.printf "%s\t%s\n" fqn sup)

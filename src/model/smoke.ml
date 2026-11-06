(* Smoke test: load the whole Embox tree, link it, load the scr5_32
 * configuration, and report the resulting statistics and any issues. *)
open Base

let () =
  let argv = Sys.get_argv () in
  let root =
    if Array.length argv > 1 then Array.get argv 1
    else "/home/kakadu/asp/embox-rukaml"
  in
  let t = Registry.load ~root in
  let mts = Registry.all_module_types t in
  Stdlib.Format.printf "module types : %d\n" (List.length mts);
  Stdlib.Format.printf "features     : %d\n" (List.length (Registry.all_features t));
  Stdlib.Format.printf "ann. types   : %d\n"
    (List.length (Registry.all_annotation_types t));
  let issues = Registry.issues t in
  Stdlib.Format.printf "issues       : %d\n" (List.length issues);
  List.iter issues ~f:(fun i -> Stdlib.Format.printf "  %s\n" (Model.show_issue i));
  let cfg = Registry.load_config t "conf/mods.conf" in
  Stdlib.Format.printf "config       : %s (%d includes)\n" cfg.cfg_name
    (List.length cfg.cfg_includes);
  if Registry.has_errors t then (
    Stdlib.Format.eprintf "RESULT: ERRORS PRESENT\n";
    Stdlib.exit 1)
  else (
    Stdlib.Format.printf "RESULT: OK (no errors)\n";
    Stdlib.exit 0)

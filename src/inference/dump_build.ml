(* Validation: run the build inference and print the ordered module list
 * (fqn + inclusion reason) to stdout, issues to stderr.
 *
 * The stdout format matches /tmp/opencode/oracle_modules.tsv so the result
 * can be diffed against the legacy build's ground truth. *)
open Base

let () =
  let argv = Sys.get_argv () in
  let root =
    if Array.length argv > 1 then Array.get argv 1
    else "/home/kakadu/asp/embox-rukaml"
  in
  let t = Registry.load ~root in
  let cfg = Registry.load_config t "conf/mods.conf" in
  let b = Build.build ~reg:t ~cfg:cfg in
  List.iter b.bd_modules ~f:(fun inst ->
    let reason =
      match inst.mi_include_member with
      | Some _ -> "explicit"
      | None -> "dep"
    in
    Stdlib.Format.printf "%s\t%s\n" (Model.fqn_to_string inst.mi_type.mt_fqn) reason);
  Stdlib.Format.eprintf "# modules: %d\n" (List.length b.bd_modules);
  let n_explicit =
    List.length
      (List.filter b.bd_modules ~f:(fun inst -> Option.is_some inst.mi_include_member))
  in
  Stdlib.Format.eprintf "# explicit: %d, dep: %d\n" n_explicit
    (List.length b.bd_modules - n_explicit);
  Stdlib.Format.eprintf "# issues: %d\n" (List.length b.bd_issues);
  List.iter b.bd_issues ~f:(fun i -> Stdlib.Format.eprintf "  %s\n" (Model.show_issue i));
  let has_error =
    List.exists b.bd_issues ~f:(fun i -> Poly.equal i.iss_severity Model.Error)
  in
  if has_error then (
    Stdlib.Format.eprintf "RESULT: ERRORS PRESENT\n";
    Stdlib.exit 1)
  else (
    Stdlib.Format.eprintf "RESULT: OK\n";
    Stdlib.exit 0)

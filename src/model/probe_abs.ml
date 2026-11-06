open Base
let () =
  let argv = Sys.get_argv () in
  let root =
    if Array.length argv > 1 then Array.get argv 1
    else "/home/kakadu/asp/embox-rukaml"
  in
  let t = Registry.load ~root in
  let abs =
    List.filter (Registry.all_module_types t) ~f:(fun mt -> mt.mt_is_abstract)
  in
  Stdlib.Format.eprintf "abstract count: %d\n" (List.length abs);
  let check = [
    "embox.kernel.task.resource.vfork";
    "embox.kernel.sched.current.api";
    "embox.kernel.spinlock";
    "embox.arch.system";
  ] in
  List.iter check ~f:(fun name ->
    let fqn = Model.fqn_of_string name in
    let mt =
      List.find (Registry.all_module_types t) ~f:(fun m -> Model.fqn_equal m.mt_fqn fqn)
    in
    (match mt with
     | Some m ->
         Stdlib.Format.eprintf "%s: abstract=%b subs=%d\n" name m.mt_is_abstract
           (List.length m.mt_sub_types)
     | None -> Stdlib.Format.eprintf "%s: NOT FOUND\n" name))

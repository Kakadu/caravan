(* mybuild.exe — the Mybuild/dune build tool.
 *
 * Usage:
 *   mybuild.exe confload <root> [template]
 *       List configuration templates, or load <template> into conf/
 *       (replacement for 'make confload-<template>').
 *   mybuild.exe gen-dune <root>
 *       Generate the dune workspace (build-dune/dune, helper scripts,
 *       gen/ files) for the configured platform.
 *   mybuild.exe mod-list <root>
 *       List all module instances of the build, one per line.
 *   mybuild.exe mod-brief <root> <module>
 *       Print a short summary of one module instance.
 *   mybuild.exe mod-include-reason <root> <module>
 *       Explain why a module is part of the build (chain of depends
 *       back to a module included from conf).
 *
 * Written in Base (not Core) per the Core -> Base migration plan.
 *)
open Base

let die fmt =
  Stdlib.Format.kasprintf (fun s ->
      Stdlib.output_string Stdlib.stderr s;
      Stdlib.exit 2) fmt

let load (root : string) : Model.build =
  let t = Registry.load ~root in
  let cfg = Registry.load_config t "conf/mods.conf" in
  Build.build ~reg:t ~cfg:cfg

let find_module (b : Model.build) (path : string) : Model.module_instance =
  match
    List.find b.bd_modules ~f:(fun inst ->
      String.equal (Model.mi_path inst) path)
  with
  | Some inst -> inst
  | None -> die "module %s is not part of the build\n" path

(* ------------------------------------------------------------------ *)
(* gen-dune                                                            *)
(* ------------------------------------------------------------------ *)

let cmd_gen_dune (root : string) : unit =
  let mods_conf = root ^ "/conf/mods.conf" in
  if not (Stdlib.Sys.file_exists mods_conf) then
    die
      "conf/mods.conf not found — load a template first:\n\
       \   mybuild.exe confload %s [<template>]\n"
      root;
  let b = load root in
  Dune_gen.emit_all root b

(* ------------------------------------------------------------------ *)
(* mod-list                                                            *)
(* ------------------------------------------------------------------ *)

let cmd_mod_list (root : string) : unit =
  let b = load root in
  List.iter b.bd_modules ~f:(fun inst ->
    Stdlib.print_endline (Model.mi_path inst))

(* ------------------------------------------------------------------ *)
(* mod-brief                                                           *)
(* ------------------------------------------------------------------ *)

let cmd_mod_brief (root : string) (path : string) : unit =
  let b = load root in
  let inst = find_module b path in
  let t = inst.mi_type in
  Stdlib.Format.printf "module %s\n%!" path;
  Stdlib.Format.printf "  type:    %s\n%!" (Model.fqn_to_string t.mt_fqn);
  Stdlib.Format.printf "  static:  %b\n%!" t.mt_is_static;
  Stdlib.Format.printf "  depends: %s\n%!"
    (String.concat ~sep:", " (List.map inst.mi_depends ~f:Model.mi_path));
  (match inst.mi_include_member with
   | Some ci ->
       Stdlib.Format.printf "  include: %s\n%!"
         (Model.show_origin ci.ci_origin)
   | None -> ())

(* ------------------------------------------------------------------ *)
(* mod-include-reason                                                  *)
(* ------------------------------------------------------------------ *)

(** Reverse depends: for every module, the list of modules that depend
 *  on it. *)
let reverse_deps (b : Model.build) : (string, string list) Hashtbl.t =
  let h = Hashtbl.create ~size:1024 (module String) in
  List.iter b.bd_modules ~f:(fun inst ->
    let p = Model.mi_path inst in
    List.iter inst.mi_depends ~f:(fun dep ->
      let dp = Model.mi_path dep in
      let cur =
        match Hashtbl.find h dp with
        | Some xs -> xs
        | None -> []
      in
      if not (List.mem cur p ~equal:String.equal) then
        Hashtbl.set h ~key:dp ~data:(p :: cur)));
  h

(** One chain from the module back to a conf-included module:
 *  [(module, reason)] pairs, target first. *)
let include_chain (b : Model.build) (rev : (string, string list) Hashtbl.t)
    (path : string) :
    (string * string) list option =
  let rec walk (p : string) (acc : (string * string) list) (visited : string list) :
      (string * string) list option =
    if List.mem visited p ~equal:String.equal then None
    else
      let cur = find_module b p in
      (match cur.mi_include_member with
       | Some ci ->
           Some
             ((p, "included from " ^ Model.show_origin ci.ci_origin)
             :: acc)
       | None ->
           (match Hashtbl.find rev p with
            | Some parents ->
                List.find_map parents ~f:(fun par ->
                  walk par ((p, "depended on by " ^ par) :: acc) (p :: visited))
            | None -> None))
  in
  walk path [] []

let cmd_mod_include_reason (root : string) (path : string) : unit =
  let b = load root in
  let rev = reverse_deps b in
  (match include_chain b rev path with
   | Some chain ->
       Stdlib.Format.printf "%s is in the build because:\n%!" path;
       List.iteri chain ~f:(fun i (p, reason) ->
         Stdlib.Format.printf "  %d. %s — %s\n%!" (i + 1) p reason)
   | None ->
       Stdlib.Format.printf
         "%s is in the build but no chain to a conf include was found \
          (mandatory module or cycle)\n%!"
         path)

(* ------------------------------------------------------------------ *)
(* main                                                                *)
(* ------------------------------------------------------------------ *)

let usage () =
  Stdlib.output_string Stdlib.stderr
    "usage: mybuild.exe <command> <root> [args]\n\
     commands:\n\
       confload [template]      load a configuration template (or list them)\n\
       gen-dune                 generate the dune workspace\n\
       mod-list                 list build modules\n\
       mod-brief <module>       module summary\n\
       mod-include-reason <module>  why the module is in the build\n"

let () =
  let argv = Sys.get_argv () in
  if Array.length argv < 3 then usage ()
  else
    let cmd = argv.(1) in
    let root = argv.(2) in
    (match cmd with
     | "confload" ->
         (if Array.length argv < 4 then Confload.list_templates root
          else Confload.cmd_conf_load root argv.(3))
     | "gen-dune" -> cmd_gen_dune root
     | "mod-list" -> cmd_mod_list root
     | "mod-brief" ->
         (if Array.length argv < 4 then usage ()
          else cmd_mod_brief root argv.(3))
     | "mod-include-reason" ->
         (if Array.length argv < 4 then usage ()
          else cmd_mod_include_reason root argv.(3))
     | _ -> usage ())

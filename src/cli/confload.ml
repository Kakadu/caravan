(* confload: template discovery and loading.
 *
 * OCaml replacement for the legacy 'make confload' /
 * 'make confload-<template>' targets (mk/main.mk). A template is any
 * directory containing a build.conf, discovered in the same places
 * with the same naming rules as the legacy build:
 *
 *   templates/**/build.conf                       -> <rel path>
 *   platform/**/build.conf                        -> platform/<rel path minus 'templates/'>
 *   project/<vendor>/templates/<n>/build.conf     -> project/<vendor>/<n>
 *   ext_project/<vendor>/templates/<n>/build.conf -> ext_project/<vendor>/<n>
 *
 * Loading a template replaces conf/ with a copy of the template
 * directory and removes the legacy build/ tree (as the legacy target
 * does, so stale artifacts never mix with the new configuration). It
 * also wipes the dune build system's generated tree (build-dune/),
 * which gen-dune recreates. The root dune/dune-project files are
 * static (template-independent) and are left in place.
 *)

open Base

type template = { name : string; dir : string }

let is_real_dir (p : string) : bool =
  try
    let st = Unix.lstat p in
    (match st.st_kind with Unix.S_DIR -> true | _ -> false)
  with Unix.Unix_error (_, _, _) -> false

let dir_entries (dir : string) : string list =
  if not (is_real_dir dir) then []
  else
    let dh = Unix.opendir dir in
    let entries = ref [] in
    (try
       while true do
         entries := Unix.readdir dh :: !entries
       done
     with End_of_file | Unix.Unix_error (_, _, _) -> ());
    Unix.closedir dh;
    (* NB: '='/'<>' are shadowed to int-only by this Base build; use
     * String.equal for strings. *)
    List.filter ~f:(fun e ->
      not (String.equal e ".") && not (String.equal e ".."))
      (List.sort ~compare:String.compare !entries)

(** Recursively collect all files named [build.conf] under [dir].
 *  Symlinks are not followed (matching make's wildcard behavior). *)
let rec collect_build_confs (dir : string) (acc : string list) : string list =
  List.fold_left ~init:acc ~f:(fun acc e ->
    let p = dir ^ "/" ^ e in
    if is_real_dir p then collect_build_confs p acc
    else if String.equal e "build.conf" && Stdlib.Sys.file_exists p then p :: acc
    else acc)
    (dir_entries dir)

let strip_prefix (prefix : string) (s : string) : string =
  let n = String.length prefix in
  if String.is_prefix s ~prefix then String.sub s ~pos:n ~len:(String.length s - n)
  else s

(* Legacy naming (mk/main.mk): every 'templates/' substring is removed
 * from the platform/project-relative path (SUBPLATFORM_TEMPLATE_DIR). *)
let drop_templates_segments (s : string) : string =
  let pat = "templates/" in
  let n = String.length pat in
  let len = String.length s in
  let b = Stdlib.Buffer.create len in
  let i = ref 0 in
  while !i < len do
    if !i + n <= len && String.equal (String.sub s ~pos:!i ~len:n) pat
    then i := !i + n
    else (
      Stdlib.Buffer.add_substring b s !i 1;
      i := !i + 1)
  done;
  Stdlib.Buffer.contents b

let discover_templates (root : string) : template list =
  let tpls =
    List.map ~f:(fun p ->
      let dir = Stdlib.Filename.dirname p in
      { name = strip_prefix (root ^ "/templates/") dir; dir })
      (collect_build_confs (root ^ "/templates") [])
    @ List.map ~f:(fun p ->
         let dir = Stdlib.Filename.dirname p in
         let rel = strip_prefix (root ^ "/platform/") dir in
         { name = "platform/" ^ drop_templates_segments rel; dir })
      (collect_build_confs (root ^ "/platform") [])
    @ List.concat_map ~f:(fun vendor ->
         let tdir = root ^ "/project/" ^ vendor ^ "/templates" in
         List.filter_map ~f:(fun n ->
           let bc = tdir ^ "/" ^ n ^ "/build.conf" in
           if Stdlib.Sys.file_exists bc
           then Some { name = "project/" ^ vendor ^ "/" ^ n; dir = tdir ^ "/" ^ n }
           else None)
           (dir_entries tdir))
         (dir_entries (root ^ "/project"))
    @ List.concat_map ~f:(fun vendor ->
         let tdir = root ^ "/ext_project/" ^ vendor ^ "/templates" in
         List.filter_map ~f:(fun n ->
           let bc = tdir ^ "/" ^ n ^ "/build.conf" in
           if Stdlib.Sys.file_exists bc
           then Some { name = "ext_project/" ^ vendor ^ "/" ^ n; dir = tdir ^ "/" ^ n }
           else None)
           (dir_entries tdir))
         (dir_entries (root ^ "/ext_project"))
  in
  List.sort ~compare:(fun a b -> String.compare a.name b.name) tpls

(** Remove [p] and everything under it. Symlinks are unlinked, not
 *  followed (the legacy target removes a conf/ symlink as a link).
 *  Does nothing if [p] does not exist. *)
let rec rm_rf (p : string) : unit =
  if not (Stdlib.Sys.file_exists p) then ()
  else
    let st = Unix.lstat p in
    (match st.st_kind with
     | Unix.S_DIR ->
         let () = List.iter ~f:(fun e -> rm_rf (p ^ "/" ^ e)) (dir_entries p) in
         Unix.rmdir p
     | _ -> Unix.unlink p)

(** Copy file contents (Unix.copy_file is absent in this toolchain).
 *  [In_channel.input] returns 0 at EOF (it does not raise), so the
 *  loop must terminate on n = 0. *)
let copy_file_contents (src : string) (dst : string) : unit =
  let ic = In_channel.open_bin src in
  let oc = Out_channel.open_bin dst in
  let buf = Bytes.create 65536 in
  (try
     let n = ref (Bytes.length buf) in
     while !n > 0 do
       n := In_channel.input ic buf 0 (Bytes.length buf);
       if !n > 0 then Out_channel.output oc buf 0 !n
     done
   with End_of_file -> ());
  In_channel.close_noerr ic;
  Out_channel.close_noerr oc

(** Copy the contents of [src] into [dst] (both must exist),
 *  preserving file modes, like 'cp -fR src/* dst'. *)
let rec copy_tree_contents (src : string) (dst : string) : unit =
  List.iter ~f:(fun e ->
    let sp = src ^ "/" ^ e in
    let dp = dst ^ "/" ^ e in
    let st = Unix.lstat sp in
    (match st.st_kind with
     | Unix.S_DIR ->
         let () = Unix.mkdir dp st.st_perm in
         copy_tree_contents sp dp
     | _ ->
         let () = copy_file_contents sp dp in
         Unix.chmod dp st.st_perm))
    (dir_entries src)

let list_templates (root : string) : unit =
  let tpls = discover_templates root in
  if List.is_empty tpls then Stdlib.print_endline "No templates are available."
  else
    (List.iter ~f:(fun t -> Stdlib.print_endline t.name) tpls;
     Stdlib.print_endline "";
     Stdlib.print_endline
       "Use 'mybuild.exe confload <root> <template>' to load one.")

let cmd_conf_load (root : string) (name : string) : unit =
  let tpls = discover_templates root in
  (match List.find ~f:(fun t -> String.equal t.name name) tpls with
   | Some t ->
       let conf = root ^ "/conf" in
       let () = rm_rf conf in
       let () = Unix.mkdir conf 0o755 in
       let () = copy_tree_contents t.dir conf in
       (* Legacy parity: wipe the legacy build tree so stale artifacts
        * never mix with the new configuration. *)
       let () = rm_rf (root ^ "/build") in
       (* Wipe the dune build system's generated tree. *)
       let () = rm_rf (root ^ "/build-dune") in
       Stdlib.print_endline "Config complete"
   | None ->
       Stdlib.Format.eprintf "Unknown template: %s\n" name;
       Stdlib.Format.eprintf "Available templates:\n%!";
       List.iter ~f:(fun t -> Stdlib.Format.eprintf "  %s\n%!" t.name) tpls;
       Stdlib.exit 1)

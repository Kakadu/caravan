(* Emit the generated C/H sources into the given output directory.
 *
 * Usage: gen_sources.exe <root> <out_dir> <config_lds_h>
 *
 * <root>          repository root (contains Mybuild files and conf/)
 * <out_dir>       directory standing in for build/base/gen
 * <config_lds_h>  path to config.lds.h (used by sections.c to test for
 *                 external-memory sections)
 *
 * Generates:
 *   <out_dir>/src/framework/mod/mod_inject.c
 *   <out_dir>/src/framework/mod/apps_build_deps.c
 *   <out_dir>/src/mem/loadsect/sections.c
 *)
open Base

let write_file path content =
  let dir = Stdlib.Filename.dirname path in
  let rec mkdirs p =
    if Stdlib.Sys.file_exists p then ()
    else
      let parent = Stdlib.Filename.dirname p in
      if String.equal parent p then ()
      else (
        mkdirs parent;
        Unix.mkdir p 0o755)
  in
  mkdirs dir;
  let oc = Stdlib.open_out_bin path in
  Stdlib.Out_channel.output_string oc content;
  Stdlib.close_out oc

let read_file path =
  let ic = Stdlib.open_in_bin path in
  let len = Stdlib.in_channel_length ic in
  let buf = Bytes.create len in
  Stdlib.really_input ic buf 0 len;
  Stdlib.close_in ic;
  Bytes.to_string buf

let () =
  let argv = Sys.get_argv () in
  if Array.length argv < 4 then (
    Stdlib.output_string Stdlib.stderr
      (argv.(0) ^ " <root> <out_dir> <config_lds_h>\n");
    Stdlib.exit 2);
  let root = argv.(1) in
  let out_dir = argv.(2) in
  let config_lds_h =
    if String.equal argv.(3) "-" then ""
    else read_file argv.(3)
  in
  let t = Registry.load ~root in
  let cfg = Registry.load_config t "conf/mods.conf" in
  let b = Build.build ~reg:t ~cfg:cfg in

  write_file (out_dir ^ "/src/framework/mod/mod_inject.c") (Modinject.render b);
  write_file (out_dir ^ "/src/framework/mod/apps_build_deps.c")
    (Sections.apps_build_deps_c b);
  write_file (out_dir ^ "/src/framework/mod/mod_info.h") (Modinfo.render b);
  write_file (out_dir ^ "/src/mem/loadsect/sections.c")
    (Sections.sections_c b config_lds_h);
  Stdlib.Format.printf "generated sources\n%!"

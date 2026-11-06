(* Emit the linker-script fragment headers into the given output directory.
 *
 * Usage: gen_lds.exe <root> <out_dir>
 *
 * <root>     repository root (contains mk/, conf/)
 * <out_dir>  directory standing in for build/base/gen
 *
 * Generates:
 *   <out_dir>/config.lds.h
 *   <out_dir>/regions.lds.h
 *   <out_dir>/sections.lds.h
 *   <out_dir>/phdrs.lds.h
 *   <out_dir>/section_symbols.lds.h
 *   <out_dir>/apps.lds.h
 *   <out_dir>/buildinfo.ld
 *   <out_dir>/image.lds.S
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

let () =
  let argv = Sys.get_argv () in
  if Array.length argv < 3 then (
    Stdlib.output_string Stdlib.stderr (argv.(0) ^ " <root> <out_dir>\n");
    Stdlib.exit 2);
  let root = argv.(1) in
  let out_dir = argv.(2) in
  let t = Registry.load ~root in
  let cfg = Registry.load_config t "conf/mods.conf" in
  let b = Build.build ~reg:t ~cfg:cfg in

  let config_lds_h = Lds.config_lds_h root in
  write_file (out_dir ^ "/config.lds.h") config_lds_h;
  write_file (out_dir ^ "/regions.lds.h") (Lds.regions_lds_h config_lds_h);
  write_file (out_dir ^ "/sections.lds.h") (Lds.sections_lds_h config_lds_h);
  write_file (out_dir ^ "/phdrs.lds.h") (Lds.phdrs_lds_h config_lds_h);
  write_file (out_dir ^ "/section_symbols.lds.h")
    (Lds.section_symbols_lds_h b config_lds_h);
  write_file (out_dir ^ "/apps.lds.h") (Lds.apps_lds_h b config_lds_h);
  write_file (out_dir ^ "/buildinfo.ld") (Lds.buildinfo_ld ());
  write_file (out_dir ^ "/image.lds.S") (Lds.image_lds_s root);
  Stdlib.Format.printf "generated lds files\n%!"

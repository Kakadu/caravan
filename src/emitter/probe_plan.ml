(* Probe: dump the build plan for cross-checking against the oracle.
 *
 * Usage: probe_plan.exe <root>
 *)
open Base

let kind_string = function
  | Buildplan.Cc -> "cc"
  | Buildplan.Lds -> "lds"
  | Buildplan.Archive -> "a"
  | Buildplan.Object -> "o"
  | Buildplan.Initfs -> "initfs"
  | Buildplan.Other -> "other"

let () =
  let argv = Sys.get_argv () in
  if Array.length argv < 2 then (
    Stdlib.output_string Stdlib.stderr (argv.(0) ^ " <root>\n");
    Stdlib.exit 2);
  let root = argv.(1) in
  let t = Registry.load ~root in
  let cfg = Registry.load_config t "conf/mods.conf" in
  let b = Build.build ~reg:t ~cfg:cfg in
  let p = Buildplan.plan_build root b in

  Stdlib.Format.printf "CC: %s\n%!" p.p_flags.gf_cc;
  Stdlib.Format.printf "CFLAGS: %s\n%!" p.p_flags.gf_cflags;
  Stdlib.Format.printf "CPPFLAGS: %s\n%!" p.p_flags.gf_cppflags;
  Stdlib.Format.printf "ASFLAGS: %s\n%!" p.p_flags.gf_asflags;
  Stdlib.Format.printf "LDFLAGS: %s\n%!" p.p_flags.gf_ldflags;

  List.iter p.p_modules ~f:(fun m ->
    Stdlib.Format.printf "MODULE %s %s static=%b stage=%d app=%b\n%!"
      m.mp_path m.mp_id m.mp_is_static m.mp_stage m.mp_is_app;
    List.iter m.mp_sources ~f:(fun s ->
      Stdlib.Format.printf "  SRC %s%s %s -> %s\n%!"
        (kind_string s.sp_kind)
        (if s.sp_owner then "" else "~")
        s.sp_file s.sp_obj_rel;
      if not (String.is_empty s.sp_flags) || not (String.is_empty s.sp_flags_before)
      then
        Stdlib.Format.printf "    FB: %s\n    FL: %s\n%!" s.sp_flags_before s.sp_flags;
      (match s.sp_rule with
       | Some r -> Stdlib.Format.printf "    RULE: %s\n%!" r
       | None -> ());
      (match s.sp_initfs_target with
       | Some tgt ->
           Stdlib.Format.printf "    INITFS: %s chmod=%s\n%!" tgt
             (Option.value ~default:"-" s.sp_initfs_chmod)
       | None -> ())));
  Stdlib.Format.printf "LDS_SCRIPTS: %s\n%!"
    (String.concat ~sep:" " p.p_lds_scripts);
  List.iter p.p_initfs ~f:(fun (src, tgt, chmod) ->
    Stdlib.Format.printf "INITFS_FILE %s -> %s chmod=%s\n%!" src tgt
      (Option.value ~default:"-" chmod));
  Stdlib.Format.printf "INITFS: %d files\n%!" (List.length p.p_initfs);
  Stdlib.Format.printf "LD_MODULES: %d  AR_MODULES: %d\n%!"
    (List.length p.p_ld_modules) (List.length p.p_ar_modules);
   Stdlib.Format.printf "LD_ORDER: %s\n%!"
     (String.concat ~sep:" " (List.map p.p_ld_modules ~f:(fun m -> m.mp_path)));
   Stdlib.Format.printf "AR_ORDER: %s\n%!"
     (String.concat ~sep:" " (List.map p.p_ar_modules ~f:(fun m -> m.mp_path)))

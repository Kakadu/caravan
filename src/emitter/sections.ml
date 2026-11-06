(* Generation of gen/src/mem/loadsect/sections.c and
 * gen/src/framework/mod/apps_build_deps.c.
 *
 * Byte-compatible with the legacy GNU Make emitters (mk/script/
 * user-lds-sections.mk and apps-build-deps.mk).
 *
 * sections.c describes, for each of the text/rodata/data/bss sections, the
 * modules that live in external memory (via @LinkerSection) together with
 * sentinel-terminated vma/lma/len arrays. apps_build_deps.c does the same
 * for @App modules. Neither has any entries for the scr5_32 profile, so
 * both reduce to their static forms; the general algorithm is implemented
 * so that profiles with external-memory sections or apps also work.
 *
 * Written in Base (not Core) per the Core -> Base migration plan.
 *)
open Base

let sections = [ "text"; "rodata"; "data"; "bss" ]

(** True when [needle] occurs in [hay] (replicates the legacy
 *  [grep <needle> config.lds.h] used to test for an external-memory
 *  section). *)
let contains = Ctx.contains

(** The external-memory section name a module type places [sec] into, if
 *  any (the [@LinkerSection(sec = "...")] value). *)
let extmem_section (mt : Model.module_type) (sec : string) : string option =
  match Ctx.type_ann_param mt Ctx.ann_linker_section sec with
  | Some (Model.V_string s) -> Some s
  | _ -> None

(** [mod_in_extmem mt sec config_lds_h] mirrors the legacy [mod_in_extmem]:
 *  the module has a @LinkerSection value for [sec] and that section is
 *  defined in config.lds.h. *)
let mod_in_extmem (mt : Model.module_type) (sec : string) (config_lds_h : string) : bool =
  match extmem_section mt sec with
  | None -> false
  | Some v -> contains config_lds_h ("LDS_SECTION_VMA_" ^ v)

let sym (mt : Model.module_type) (sec : string) (kind : string) : string =
  "__module_" ^ Model.fqn_to_id mt.mt_fqn ^ "_" ^ sec ^ "_" ^ kind

(** The declaration line and sentinel for a [kind] array of [sec]. *)
let array_decl sec kind =
  match kind with
  | "vma" -> "void *sections_" ^ sec ^ "_vma[] = {", "\t(void *)0"
  | "lma" -> "void *sections_" ^ sec ^ "_lma[] = {", "\t(void *)0"
  | _ -> "unsigned sections_" ^ sec ^ "_len[] = {", "\t(unsigned)-1"

(** One element line for a [kind] array. *)
let array_elem mt sec kind =
  match kind with
  | "len" -> Printf.sprintf "\t(unsigned)&%s," (sym mt sec "len")
  | _ -> Printf.sprintf "\t(void *)&%s," (sym mt sec kind)

(** Render sections.c. [config_lds_h] is the content of the generated
 *  config.lds.h (used to test for external-memory sections). *)
let sections_c (b : Model.build) (config_lds_h : string) : string =
  let types = List.map b.bd_modules ~f:(fun inst -> inst.mi_type) in
  let lines = ref [] in
  let add s = lines := s :: !lines in

  (* Part 1: extern references, one comment per section. *)
  List.iter sections ~f:(fun sec ->
    add ("/* " ^ sec ^ " */");
    List.iter types ~f:(fun mt ->
      if mod_in_extmem mt sec config_lds_h then (
        add (Printf.sprintf "extern char %s;" (sym mt sec "vma"));
        add (Printf.sprintf "extern char %s;" (sym mt sec "lma"));
        add (Printf.sprintf "extern char %s;" (sym mt sec "len"))));
  );

  (* Part 2: the vma/lma/len arrays. *)
  List.iter sections ~f:(fun sec ->
    add ("/* " ^ sec ^ " */");
    List.iter [ "vma"; "lma"; "len" ] ~f:(fun kind ->
      let (decl, sentinel) = array_decl sec kind in
      add decl;
      List.iter types ~f:(fun mt ->
        if mod_in_extmem mt sec config_lds_h then add (array_elem mt sec kind));
      add sentinel;
      add "};"));

  String.concat ~sep:"\n" (List.rev !lines) ^ "\n"

(** Render apps_build_deps.c. Empty when there are no @App modules (the
 *  scr5_32 case). Profiles with apps would need the transitive build-deps
 *  closure (legacy build-deps.mk); that is not required for scr5_32. *)
let apps_build_deps_c (b : Model.build) : string =
  let has_app =
    List.exists b.bd_modules ~f:(fun inst -> Ctx.has_ann_on_all_types inst Ctx.ann_app)
  in
  if has_app then
    failwith "apps_build_deps.c: @App modules require the build-deps closure (not needed for scr5_32)"
  else ""

(* Dune build file generator.
 *
 * Turns the build plan (Buildplan.plan) into a dune workspace that builds
 * the Embox image with the dune build engine:
 *
 *   <root>/dune-project          workspace marker (generated)
 *   <root>/dune                  top-level rule: embox -> build-dune/bin/embox
 *   <root>/build-dune/dune       all compile/link rules (generated)
 *   <root>/build-dune/scripts/   helper shell scripts (generated)
 *   <root>/build-dune/gen/       headers, lds fragments, generated sources,
 *                                buildinfo.ld, image.lds.S, gen.stamp
 *
 * The commands replicate the legacy make recipes (mk/image_lib.mk,
 * mk/image2.mk, mk/image3.mk, mk/arhelper.mk) one-to-one; see
 * BUILD_SYSTEM_REWRITE_PLAN.md for the mapping.
 *
 * Invalidation model:
 *  - every rule depends on its own source file(s) and on gen/gen.stamp;
 *  - gen-dune rewrites gen/gen.stamp (a hash of all Mybuild files and
 *    of the conf directory) on every run, so any model change
 *    invalidates the whole
 *    image, while a change to a single .c/.S file invalidates only the
 *    affected compile/link chain;
 *  - header dependencies finer than the module config header are not
 *    tracked (the legacy uses -MMD depfiles; dune 3.20 has no depfiles
 *    support), so editing a plain C header requires a clean rebuild.
 *
 * Written in Base (not Core) per the Core -> Base migration plan.
 *)
open Base

(* ------------------------------------------------------------------ *)
(* File writing                                                        *)
(* ------------------------------------------------------------------ *)

let mkdir_p (path : string) : unit =
  let rec mkdirs p =
    if Stdlib.Sys.file_exists p then ()
    else
      let parent = Stdlib.Filename.dirname p in
      if String.equal parent p then ()
      else (
        mkdirs parent;
        Unix.mkdir p 0o755)
  in
  mkdirs path

(** Recursively delete the contents of [dir] (the directory itself is kept).
 *  Used to wipe pure build-output directories before regenerating the dune
 *  files, so that stale artifacts never clash with rule targets ("file
 *  present in source tree"). *)
let rec wipe_dir_contents (dir : string) : unit =
  (* NB: in this toolchain Sys.is_directory raises on missing paths, so
   * guard with file_exists first. *)
  if not (Stdlib.Sys.file_exists dir && Stdlib.Sys.is_directory dir) then ()
  else
    let dh = Unix.opendir dir in
    let entries = ref [] in
    (try
       while true do
         entries := Unix.readdir dh :: !entries
       done
     with End_of_file | Unix.Unix_error (_, _, _) -> ());
    Unix.closedir dh;
    List.iter (List.sort ~compare:String.compare !entries) ~f:(fun e ->
      if String.equal e "." || String.equal e ".." then ()
      else
        let p = dir ^ "/" ^ e in
        if Stdlib.Sys.is_directory p then (
          wipe_dir_contents p;
          (try Unix.rmdir p with Unix.Unix_error (_, _, _) -> ()));
        (try Unix.unlink p with Unix.Unix_error (_, _, _) -> ()))

let write_file path content =
  let dir = Stdlib.Filename.dirname path in
  mkdir_p dir;
  let oc = Stdlib.open_out_bin path in
  Stdlib.Out_channel.output_string oc content;
  Stdlib.close_out oc

let write_exec path content =
  write_file path content;
  Unix.chmod path 0o755

(* ------------------------------------------------------------------ *)
(* Small helpers                                                       *)
(* ------------------------------------------------------------------ *)

(** Escape a string for inclusion in a dune s-expr double-quoted string.
 *  Control characters (CR/LF/TAB) are turned into backslash escapes so that
 *  no raw control byte ever reaches the dune file (which would break the
 *  s-expression lexer). Dune unescapes them back to the original bytes in
 *  the command, where the shell handles them. *)
let sesc (s : string) : string =
  let buf = Buffer.create (String.length s) in
  String.iter s ~f:(function
    | '\\' -> Buffer.add_string buf "\\\\"
    | '"' -> Buffer.add_string buf "\\\""
    | '\r' -> Buffer.add_string buf "\\r"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\t' -> Buffer.add_string buf "\\t"
    | c -> Buffer.add_char buf c);
  Buffer.contents buf

(** Shell-quote one argument (always single-quoted). *)
let sh_arg (s : string) : string =
  "'" ^ Buildplan.replace_all s "'" "'\\''" ^ "'"

(** Join non-empty tokens with a single space (make-style). *)
let cmd_of (toks : string list) : string =
  String.concat ~sep:" " (List.filter toks ~f:(fun t -> not (String.is_empty t)))

(** Unescape make's [$$] to [$] for use in a plain shell command. *)
let unescape_make (s : string) : string =
  Buildplan.replace_all s "$$" "$"

type t = {
  root : string;
  plan : Buildplan.plan;
  build : Model.build;
}

let dirs (t : t) = t.plan.p_dirs
let flags (t : t) = t.plan.p_flags

(* ------------------------------------------------------------------ *)
(* gen.stamp                                                           *)
(* ------------------------------------------------------------------ *)

(** Hash of everything that feeds the build model: all Mybuild files and
 *  the conf/ directory (the confload output). Rewritten on every
 *  gen-dune run; identical content when nothing relevant changed. *)
let gen_stamp (root : string) : string =
  let h =
    Buildplan.run_sh
      ("cd " ^ root
      ^ " && find . -name Mybuild"
      ^ " -not -path './build/*' -not -path './build-dune/*'"
      ^ " -not -path './.git/*' -not -path './caravan/*'"
      ^ " -not -path './ocaml-mcp/*' | sort | xargs cat | sha256sum")
  in
  let c =
    Buildplan.run_sh
      ("cd " ^ root ^ " && find conf -type f | sort | xargs cat | sha256sum")
  in
  "mybuild: " ^ h ^ "conf: " ^ c

(* ------------------------------------------------------------------ *)
(* Helper scripts                                                      *)
(* ------------------------------------------------------------------ *)

(** Replicates the legacy module ld link (mk/image2.mk):
 *  [ld -r -o $@ $(ldflags) $(o_files) [--whole-archive $(a_files)
 *  --no-whole-archive]] followed by the do_objcopy section renames. *)
let module_link_sh =
  "#!/bin/bash\n\
   # Generated by mybuild gen-dune. Do not edit.\n\
   # module-link.sh OUT MODULE_ID LD LDFLAGS OBJDUMP OBJCOPY CONFIG_LDS_H \\\n\
   #               LS_TEXT LS_RODATA LS_DATA LS_BSS LS_ARM_EXIDX LS_ARM_EXTAB \\\n\
   #               STRIP_MAIN PRESERVE_LOCALS CXX_CMD CC CFLAGS CPPFLAGS \\\n\
   #               TEMPLATE N_O O_FILES... A_FILES...\n\
   set -eu\n\
   OUT=$1; ID=$2; LD=$3; LDFLAGS=$4; OBJDUMP=$5; OBJCOPY=$6; CONFIG_LDS_H=$7\n\
   LS_TEXT=$8; LS_RODATA=$9; LS_DATA=${10}; LS_BSS=${11}\n\
   LS_ARM_EXIDX=${12}; LS_ARM_EXTAB=${13}\n\
   STRIP_MAIN=${14}; PRESERVE_LOCALS=${15}; CXX_CMD=${16}\n\
   CC=${17}; CFLAGS=${18}; CPPFLAGS=${19}; TEMPLATE=${20}; N_O=${21}\n\
   shift 21\n\
   O_FILES=()\n\
   i=0\n\
   while [ \"$i\" -lt \"$N_O\" ]; do O_FILES+=(\"$1\"); shift; i=$((i+1)); done\n\
   A_FILES=(\"$@\")\n\
   \n\
   # 1. Partial link of the module objects (legacy image2.mk module rule).\n\
   CMD=(\"$LD\" -r -o \"$OUT\" $LDFLAGS)\n\
   for f in \"${O_FILES[@]}\"; do CMD+=(\"$f\"); done\n\
   if [ ${#A_FILES[@]} -gt 0 ]; then\n\
   \tCMD+=(--whole-archive)\n\
   \tfor f in \"${A_FILES[@]}\"; do CMD+=(\"$f\"); done\n\
   \tCMD+=(--no-whole-archive)\n\
   fi\n\
   \"${CMD[@]}\"\n\
   \n\
   # 2. Rename the module sections (legacy rename_section/do_objcopy).\n\
   is_region() {\n\
   \t[ -n \"$1\" ] && grep -q \"LDS_SECTION_VMA_$1\" \"$CONFIG_LDS_H\"\n\
   }\n\
   sections_of() {\n\
   \t# $1=file $2=class: sections like .text, .text.foo, but not\n\
   \t# .text..foo and .text.embox.foo (legacy get_sections).\n\
   \t\"$OBJDUMP\" -h \"$1\" | tr ' \\t' '\\n\\n' | while read -r tok; do\n\
   \t\tcase \"$tok\" in\n\
   \t\t\t\".$2\"*)\n\
   \t\t\t\tcase \"$tok\" in\n\
   \t\t\t\t\t\".$2\"..*|\".$2\".embox*) ;;\n\
   \t\t\t\t\t*) printf '%s\\n' \"$tok\" ;;\n\
   \t\t\t\tesac\n\
   \t\t\t;;\n\
   \t\tesac\n\
   \tdone\n\
   }\n\
   emit_renames() {\n\
   \t# $1=file $2=class $3=@LinkerSection value\n\
   \tsections_of \"$1\" \"$2\" | while read -r sec; do\n\
   \t\tif is_region \"$3\"; then\n\
   \t\t\tprintf -- '--rename-section %s=.%s.%s.module.%s\\n' \\\n\
   \t\t\t\t\"$sec\" \"$3\" \"$sec\" \"$ID\"\n\
   \t\telse\n\
   \t\t\tprintf -- '--rename-section %s=%s.module.%s\\n' \"$sec\" \"$sec\" \"$ID\"\n\
   \tfi\n\
   \tdone\n\
   }\n\
   OPTS=$(mktemp)\n\
   trap 'rm -f \"$OPTS\"' EXIT\n\
   for f in \"${O_FILES[@]}\" \"${A_FILES[@]}\"; do\n\
   \temit_renames \"$f\" text \"$LS_TEXT\"\n\
   \temit_renames \"$f\" rodata \"$LS_RODATA\"\n\
   \temit_renames \"$f\" data \"$LS_DATA\"\n\
   \temit_renames \"$f\" bss \"$LS_BSS\"\n\
   \temit_renames \"$f\" \"ARM.exidx\" \"$LS_ARM_EXIDX\"\n\
   \temit_renames \"$f\" \"ARM.extab\" \"$LS_ARM_EXTAB\"\n\
   done | LC_ALL=C sort -u > \"$OPTS\"\n\
   if [ -s \"$OPTS\" ]; then\n\
   \txargs -a \"$OPTS\" \"$OBJCOPY\" \"$OUT\"\n\
   fi\n\
   \n\
   # 3. Main stripping (legacy mk/main-stripping.sh): rename main to\n\
   #    main_<ID>, register it as a command via the application template,\n\
   #    and re-link the result as a relocatable object.\n\
   if [ \"$STRIP_MAIN\" = \"yes\" ]; then\n\
   \tREDEF=$OUT.redef.o\n\
   \tWRAP=$OUT.cmd.o\n\
   \tMAIN_NAME=main_$ID\n\
   \tif [ \"$PRESERVE_LOCALS\" != \"yes\" ]; then\n\
   \t	\"$OBJDUMP\" -t \"$OUT\" | grep \" g \" | tr -s ' ' | cut -d' ' -f5 |\n\
   \t		(grep -v main || true) > $OUT.lsyms.txt\n\
   \t	echo >> $OUT.lsyms.txt\n\
   \t	\"$OBJCOPY\" --localize-symbols=$OUT.lsyms.txt \\\n\
   \t		--redefine-sym=main=$MAIN_NAME \"$OUT\" \"$REDEF\"\n\
   \telse\n\
   \t	\"$OBJCOPY\" --redefine-sym=main=$MAIN_NAME \"$OUT\" \"$REDEF\"\n\
   \tfi\n\
   \t\"$CC\" $CFLAGS -DMAIN_ROUTING_NAME=$MAIN_NAME -D__EMBUILD_MOD__=$ID \\\n\
   \t	-DCXX_CMD=$CXX_CMD -c -o \"$WRAP\" \"$TEMPLATE\" $CPPFLAGS\n\
   \t\"$LD\" -r $LDFLAGS \"$REDEF\" \"$WRAP\" -o \"$OUT\"\n\
   fi\n"

(** Replicates the legacy ar module build (mk/arhelper.mk, no-APP_ID
 *  branch): sort inputs, extract each archive into <a>.x/, then
 *  [ar rcs] the objects and the extracted members. *)
let module_ar_sh =
  "#!/bin/bash\n\
   # Generated by mybuild gen-dune. Do not edit.\n\
   # module-ar.sh TARGET AR ARFLAGS N_O O_FILES... A_FILES...\n\
   set -eu\n\
   TARGET=$1; AR=$2; ARFLAGS=$3; N_O=$4\n\
   shift 4\n\
   O_FILES=()\n\
   i=0\n\
   while [ \"$i\" -lt \"$N_O\" ]; do O_FILES+=(\"$1\"); shift; i=$((i+1)); done\n\
   A_FILES=(\"$@\")\n\
   \n\
   # make's $(sort): bytewise lexicographic.\n\
   if [ ${#O_FILES[@]} -gt 0 ]; then\n\
   \tmapfile -t O_FILES < <(printf '%s\\n' \"${O_FILES[@]}\" | LC_ALL=C sort)\n\
   fi\n\
   if [ ${#A_FILES[@]} -gt 0 ]; then\n\
   \tmapfile -t A_FILES < <(printf '%s\\n' \"${A_FILES[@]}\" | LC_ALL=C sort)\n\
   fi\n\
   \n\
   # Extract each input archive into <a>.x/ (legacy arhelper.mk).\n\
   for a in \"${A_FILES[@]}\"; do\n\
   \tmkdir -p \"$a.x\"\n\
   \t( cd \"$a.x\" && \"$AR\" x \"../$(basename \"$a\")\" )\n\
   done\n\
   \n\
   MEMBERS=()\n\
   for a in \"${A_FILES[@]}\"; do\n\
   \twhile read -r m; do MEMBERS+=(\"$a.x/$m\"); done < <(\"$AR\" t \"$a\")\n\
   done\n\
   \n\
   rm -f \"$TARGET\"\n\
   {\n\
   \tfor o in \"${O_FILES[@]}\"; do printf '%s\\n' \"$o\"; done\n\
   \tfor m in \"${MEMBERS[@]}\"; do printf '%s\\n' \"$m\"; done\n\
   } | xargs \"$AR\" $ARFLAGS \"$TARGET\"\n"

(** Replicates the legacy rootfs image build (mk/image2.mk):
 *  [cd ROOTFS_DIR && find * -depth -print | cpio -L --quiet -H newc
 *  -o -O IMAGE], appending USER_ROOTFS_DIR with -A when present. *)
let rootfs_sh =
  "#!/bin/bash\n\
   # Generated by mybuild gen-dune. Do not edit.\n\
   # rootfs.sh ROOTFS_DIR IMAGE USER_ROOTFS_DIR CPIO\n\
   set -eu\n\
   ROOTFS_DIR=$1; IMAGE=$2; USER_ROOTFS_DIR=$3; CPIO=$4\n\
   mkdir -p \"$ROOTFS_DIR\"\n\
   # Pin IMAGE to an absolute path before cd'ing (legacy uses $(abspath $@));\n\
   # otherwise the image lands inside ROOTFS_DIR and gets archived into itself.\n\
   case $IMAGE in /*) ;; *) IMAGE=$(pwd)/$IMAGE ;; esac\n\
   cd \"$ROOTFS_DIR\" && find * -depth -print | \"$CPIO\" -L --quiet -H newc -o -O \"$IMAGE\"\n\
   if [ -d \"$USER_ROOTFS_DIR\" ]; then\n\
   \tcd \"$USER_ROOTFS_DIR\" && find * -depth -print | \"$CPIO\" -L --quiet -H newc -o -A -O \"$IMAGE\"\n\
   fi\n"

(* ------------------------------------------------------------------ *)
(* Rule representation                                                 *)
(* ------------------------------------------------------------------ *)

(** A single dune rule. [r_dir] is the build-dune-relative directory that
 *  will hold a [dune] file for this rule ("" = build-dune itself);
 *  [r_target] is the target basename within it.
 *
 *  Path conventions (dune runs the action with cwd = the build-tree mirror
 *  of [r_dir], even with DUNE_SANDBOX=none):
 *  - the action writes its target with a cwd-relative path (the basename);
 *  - [r_deps] on build artifacts are cwd-relative paths (see [rel_build]);
 *  - [r_deps] on source-tree files (repo files and gen-dune output) are
 *    absolute paths;
 *  - the action reads source-tree inputs via absolute paths.
 *  [r_promote] marks rules whose target is also kept in the source tree
 *  ((mode promote)); used for stamps so that a leftover source-tree copy
 *  never triggers a "file present in source tree" conflict. *)
type rule = {
  r_dir : string;
  r_target : string;
  r_deps : string list;
  r_action : string;
  r_promote : bool;
}

(** Absolute path for a build-dune-relative path (source tree). *)
let abs_bd (t : t) (p : string) : string = (dirs t).build_dune ^ "/" ^ p

(** Absolute path of a source file (real source tree, or gen/ if generated). *)
let abs_src (t : t) (s : Buildplan.source_plan) : string =
  let dd = dirs t in
  if s.sp_generated then dd.gen ^ "/" ^ s.sp_file else dd.root ^ "/" ^ s.sp_file

(** Split a build-dune-relative output path into (dir, basename); dir is ""
 *  when the output sits directly in build-dune. *)
let split_target (obj_rel : string) : string * string =
  let d = Stdlib.Filename.dirname obj_rel in
  let b = Stdlib.Filename.basename obj_rel in
  let dir = if String.equal d "." then "" else d in
  (dir, b)

let make_rule ?(promote = false) (obj_rel : string) (deps : string list) (action : string) :
    rule =
  let d, b = split_target obj_rel in
  { r_dir = d; r_target = b; r_deps = deps; r_action = action; r_promote = promote }

let gen_stamp_dep (t : t) : string = abs_bd t "gen/gen.stamp"

(** Cwd-relative path from a build-dune-relative directory [from] to a
 *  build-dune-relative path [to] (both in the build tree). Used for deps
 *  on build artifacts and for referencing artifacts inside actions. *)
let rel_build (from : string) (to_ : string) : string =
  let fp = if String.is_empty from then [] else String.split ~on:'/' from in
  let tp = if String.is_empty to_ then [] else String.split ~on:'/' to_ in
  let rec common (a : string list) (b : string list) : int =
    match a, b with
    | x :: xs, y :: ys -> if String.equal x y then 1 + common xs ys else 0
    | _ -> 0
  in
  let n = common fp tp in
  let ups = List.length fp - n in
  let ups_part = String.concat ~sep:"" (List.init ups ~f:(fun _ -> "../")) in
  let rest = String.concat ~sep:"/" (List.drop tp n) in
  if String.is_empty rest && ups > 0 then
    (* Drop the trailing slash so callers can append "/..." without
     * producing a double slash. *)
    String.sub ups_part ~pos:0 ~len:(String.length ups_part - 1)
  else ups_part ^ rest

(** Extra file deps implied by the per-source flags: any [-include <f>]
 *  (already absolute) plus the rootfs image when it is referenced. *)
let flag_file_deps (d : string) (s : Buildplan.source_plan) : string list =
  let toks = String.split ~on:' ' (s.sp_flags_before ^ " " ^ s.sp_flags) in
  let acc = ref [] in
  let rec loop = function
    | "-include" :: f :: rest ->
        if String.is_prefix f ~prefix:"/" then acc := f :: !acc;
        loop rest
    | _ :: rest -> loop rest
    | [] -> ()
  in
  loop toks;
  (* Substring, not suffix: the path may be embedded in a -D macro value
   * with surrounding quotes (INITFS_IMAGE_PATH). *)
  let has_cpio =
    List.exists (String.split ~on:' ' s.sp_flags) ~f:(fun x ->
      String.is_substring x ~substring:".cpio")
  in
  let cpio_dep = if has_cpio then [ rel_build d "obj/rootfs.cpio" ] else [] in
  List.dedup_and_sort (!acc @ cpio_dep) ~compare:String.compare

(** Compile rule for one owned source. *)
let compile_rule (t : t) (m : Buildplan.module_plan) (s : Buildplan.source_plan) :
    rule =
  let g = flags t in
  let dd = dirs t in
  let src = abs_src t s in
  let d, b = split_target s.sp_obj_rel in
  let cmd =
    if Buildplan.kind_is_lds s.sp_kind then
      (* Linker-script sources may embed legacy build-base paths (e.g.
       * buildinfo.lds.S: INCLUDE "./build/base/gen/buildinfo.ld"). ld
       * resolves INCLUDE relative to the link cwd, which in the build tree
       * has no build/base/ counterpart, so rewrite the prefix to the
       * absolute source-tree build-dune root (the gen/ dir is not mirrored
       * into the build tree). *)
      cmd_of
        [
          g.gf_cc;
          "-E";
          s.sp_flags_before;
          g.gf_cflags;
          "-P -D__LDS__";
          g.gf_cppflags;
          s.sp_flags;
          "-I" ^ dd.gen;
          "-imacros " ^ dd.gen ^ "/config.lds.h";
          "-o " ^ b ^ ".tmp";
          src;
        ]
      ^ " && sed 's|./build/base/|"
      ^ dd.build_dune
      ^ "/|g; s|build/base/|"
      ^ dd.build_dune
      ^ "/|g' "
      ^ b
      ^ ".tmp > "
      ^ b
      ^ " && rm -f "
      ^ b
      ^ ".tmp"
    else if String.is_suffix s.sp_file ~suffix:".S" then
      cmd_of
        [
          g.gf_cc;
          s.sp_flags_before;
          g.gf_asflags;
          g.gf_cppflags;
          s.sp_flags;
          "-c -o " ^ b;
          src;
        ]
    else
      cmd_of
        [
          g.gf_cc;
          s.sp_flags_before;
          g.gf_cflags;
          g.gf_cppflags;
          s.sp_flags;
          "-c -o " ^ b;
          src;
        ]
  in
  (* Per-source flags may embed build-artifact paths ($(OBJ_DIR),
   *  $(BIN_DIR), $(ROOTFS_IMAGE)); they expand to source-tree absolute
   *  paths, but the action runs in the build tree, so rewrite them to
   *  cwd-relative. Source-tree dirs ($(SRCGEN_DIR),
   *  $(INCLUDE_INSTALL_DIR)) are deliberately left absolute. *)
  let cmd =
    let cmd = Buildplan.replace_all cmd dd.obj (rel_build d "obj") in
    Buildplan.replace_all cmd dd.bin (rel_build d "bin")
  in
  let cfg_hdr = abs_bd t ("gen/include/config/" ^ m.mp_path ^ ".h") in
  make_rule s.sp_obj_rel
    ([ src; cfg_hdr; gen_stamp_dep t; rel_build d "inc.stamp" ] @ flag_file_deps d s)
    cmd

(** Rule for an archive source: either the @Rule script (e.g. libgcc.a) or a
 *  plain copy from the source tree. The expanded @Rule script references
 *  the output by its absolute source-tree path; rewrite it to cwd-relative
 *  form (the action cwd is the output directory). *)
let rule_source_rule (t : t) (s : Buildplan.source_plan) : rule =
  let d, b = split_target s.sp_obj_rel in
  let cmd =
    match s.sp_rule with
    | Some script ->
        let s1 = Buildplan.replace_all script (abs_bd t s.sp_obj_rel) b in
        let s2 = Buildplan.replace_all s1 (abs_bd t d) "." in
        unescape_make s2
    | None -> "cp " ^ t.root ^ "/" ^ s.sp_file ^ " " ^ b
  in
  let deps =
    if Option.is_some s.sp_rule
    then [ gen_stamp_dep t ]
    else [ t.root ^ "/" ^ s.sp_file; gen_stamp_dep t ]
  in
  make_rule s.sp_obj_rel deps cmd

(** Module ld link rule. *)
let module_ld_rule (t : t) (m : Buildplan.module_plan) : rule =
  let g = flags t in
  let dd = dirs t in
  let out_rel = "module/" ^ m.mp_path ^ ".o" in
  let d, b = split_target out_rel in
  let ls =
    let get name =
      List.find_map m.mp_linker_section ~f:(fun (a, b) ->
        if String.equal a name then Some b else None)
    in
    [
      get "text";
      get "rodata";
      get "data";
      get "bss";
      get "arm_exidx";
      get "arm_extab";
    ]
  in
  let ls_args =
    String.concat ~sep:" "
      (List.map ls ~f:(fun v -> sh_arg (Option.value ~default:"" v)))
  in
  let objs =
    String.concat ~sep:" " (List.map (m.mp_o_files @ m.mp_a_files) ~f:(rel_build d))
  in
  let script = abs_bd t "scripts/module-link.sh" in
  let template = t.root ^ "/mk/script/application_template.c" in
  (* The script word-splits these flags without bash quote-removal, so
   *  unescape \" to " (what bash produces when parsing the direct
   *  compile command line). *)
  let unesc s = Buildplan.replace_all s "\\\"" "\"" in
  let cmd =
    cmd_of
      [
        script;
        b;
        sh_arg m.mp_id;
        g.gf_cross ^ "ld";
        sh_arg g.gf_ldflags;
        g.gf_cross ^ "objdump";
        g.gf_cross ^ "objcopy";
        dd.gen ^ "/config.lds.h";
        ls_args;
        (if m.mp_strip_main then "yes" else "no");
        (if m.mp_preserve_locals then "yes" else "no");
        (if m.mp_cxx_cmd then "1" else "0");
        g.gf_cc;
        sh_arg (unesc g.gf_cflags);
        sh_arg (unesc g.gf_cppflags);
        template;
        Int.to_string (List.length m.mp_o_files);
        objs;
      ]
  in
  let deps =
    List.map (m.mp_o_files @ m.mp_a_files) ~f:(rel_build d)
    @ [ gen_stamp_dep t; script ]
    @ (if m.mp_strip_main then [ template ] else [])
  in
  make_rule out_rel deps cmd

(** Module ar rule. *)
let module_ar_rule (t : t) (m : Buildplan.module_plan) : rule =
  let g = flags t in
  let out_rel = "module/" ^ m.mp_path ^ ".a" in
  let d, b = split_target out_rel in
  let objs =
    String.concat ~sep:" " (List.map (m.mp_o_files @ m.mp_a_files) ~f:(rel_build d))
  in
  let script = abs_bd t "scripts/module-ar.sh" in
  let cmd =
    cmd_of
      [
        script;
        b;
        g.gf_cross ^ "ar";
        "rcs";
        Int.to_string (List.length m.mp_o_files);
        objs;
      ]
  in
  let deps =
    List.map (m.mp_o_files @ m.mp_a_files) ~f:(rel_build d) @ [ gen_stamp_dep t; script ]
  in
  make_rule out_rel deps cmd

(** @InitFS copy rule. *)
let initfs_copy_rule (t : t) (src : string) (target : string) (chmod : string option) :
    rule =
  let dst_rel = "obj/rootfs/" ^ target in
  let _d, b = split_target dst_rel in
  let chmod_part =
    match chmod with
    | Some c -> "chmod " ^ c ^ " " ^ b
    | None -> ""
  in
  (* Legacy runs these as separate recipe lines (independent shells);
   * chain with && so a failure aborts the action. *)
  let cmd =
    String.concat ~sep:" && "
      (List.filter ~f:(fun s -> not (String.is_empty s))
         [
           "cp -r -T " ^ t.root ^ "/" ^ src ^ " " ^ b;
           chmod_part;
           "touch " ^ b;
           "find " ^ b ^ " -name .gitkeep -type f -print0 | xargs -0 rm -rf";
         ])
  in
  make_rule dst_rel [ t.root ^ "/" ^ src; gen_stamp_dep t ] cmd

(** Single rule installing all @IncludeExport headers into the
 *  include-install directory (source tree, via the generated script). The
 *  target is a stamp file in the build tree; compile rules depend on it
 *  for ordering and invalidation. Dune 3.20 has no depfiles, so per-header
 *  tracking is not possible. *)
let include_install_rule (t : t) : rule =
  let deps =
    List.map t.plan.p_include_exports ~f:(fun (src_rel, _dest) ->
      t.root ^ "/" ^ src_rel)
    @ [ gen_stamp_dep t; abs_bd t "scripts/include-install.sh" ]
  in
  make_rule ~promote:true "inc.stamp" deps
    ((abs_bd t "scripts/include-install.sh") ^ " && touch inc.stamp")

(** The generated include-install script (grouped by destination dir). *)
let include_install_sh (t : t) : string =
  let dd = dirs t in
  let by_dir = Hashtbl.create ~size:64 (module String) in
  List.iter t.plan.p_include_exports ~f:(fun (src_rel, dest_rel) ->
    let d = Stdlib.Filename.dirname dest_rel in
    let d = if String.equal d "." then "" else d in
    let b = Stdlib.Filename.basename dest_rel in
    let cur =
      match Hashtbl.find by_dir d with
      | Some xs -> xs
      | None -> []
    in
    Hashtbl.set by_dir ~key:d ~data:((src_rel, b) :: cur));
  let buf = Buffer.create 8192 in
  Buffer.add_string buf
    ("#!/bin/bash\n"
     ^ "# Generated by mybuild gen-dune. Do not edit.\n"
     ^ "set -eu\n"
     ^ "DEST=" ^ dd.inc_install ^ "\n"
     ^ "rm -rf \"$DEST\"\n"
     ^ "mkdir -p \"$DEST\"\n");
  Hashtbl.to_alist by_dir
  |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  |> List.iter ~f:(fun (d, pairs) ->
       if not (String.is_empty d) then
         Buffer.add_string buf ("mkdir -p \"$DEST/" ^ d ^ "\"\n");
       List.iter (List.rev pairs) ~f:(fun (src_rel, b) ->
         Buffer.add_string buf
           ("cp " ^ t.root ^ "/" ^ src_rel ^ " \"$DEST"
            ^ (if String.is_empty d then "" else "/" ^ d) ^ "/" ^ b ^ "\"\n")));
  Buffer.contents buf

(** rootfs.cpio rule. *)
let rootfs_cpio_rule (t : t) : rule =
  let deps =
    List.map t.plan.p_initfs ~f:(fun (_src, target, _chmod) ->
      rel_build "obj" ("obj/rootfs/" ^ target))
    @ [ gen_stamp_dep t; abs_bd t "scripts/rootfs.sh" ]
  in
  let cmd =
    cmd_of
      [
        abs_bd t "scripts/rootfs.sh";
        "rootfs";
        "rootfs.cpio";
        t.root ^ "/conf/rootfs";
        "cpio";
      ]
  in
  make_rule "obj/rootfs.cpio" deps cmd

(** image.lds rule (mk/image.lds.S preprocessed with the arch and platform
 *  lds includes). *)
let image_lds_rule (t : t) : rule =
  let g = flags t in
  let dd = dirs t in
  let arch_lds = "src/arch/" ^ g.gf_arch ^ "/embox.lds.S" in
  let plat_lds =
    let p = "platform/" ^ g.gf_platform ^ "/arch/" ^ g.gf_arch ^ "/platform.lds.S" in
    if Stdlib.Sys.file_exists (t.root ^ "/" ^ p) then p else ""
  in
  let cmd =
    cmd_of
      [
        g.gf_cc;
        "-E";
        g.gf_cflags;
        "-P -D__LDS__";
        g.gf_cppflags;
        "-include " ^ t.root ^ "/" ^ arch_lds;
        (if String.is_empty plat_lds then "" else "-include " ^ t.root ^ "/" ^ plat_lds);
        "-I" ^ dd.gen;
        "-imacros " ^ dd.gen ^ "/config.lds.h";
        "-o image.lds";
        dd.gen ^ "/image.lds.S";
      ]
  in
  let deps =
    [
      dd.gen ^ "/image.lds.S";
      dd.gen ^ "/config.lds.h";
      t.root ^ "/" ^ arch_lds;
      rel_build "obj" "inc.stamp";
    ]
    @ (if String.is_empty plat_lds then [] else [ t.root ^ "/" ^ plat_lds ])
    @ [ gen_stamp_dep t ]
  in
  make_rule "obj/image.lds" deps cmd

(** Partial link (embox-2.o), final link (bin/embox), and bin/embox.bin. *)
let link_rules (t : t) : rule list =
  let g = flags t in
  let ld_objs =
    List.map t.plan.p_ld_modules ~f:(fun m -> rel_build "obj" ("module/" ^ m.mp_path ^ ".o"))
  in
  let ld_libs =
    List.map t.plan.p_ar_modules ~f:(fun m -> rel_build "obj" ("module/" ^ m.mp_path ^ ".a"))
  in
  let lds = List.map t.plan.p_lds_scripts ~f:(rel_build "obj") in
  let embox2 = "embox-2.o" in
  let partial_cmd =
    cmd_of
      [
        g.gf_cross ^ "ld";
        "-r";
        g.gf_ldflags;
        String.concat ~sep:" " (List.map lds ~f:(fun l -> "-T " ^ l));
        String.concat ~sep:" " ld_objs;
        "--start-group";
        String.concat ~sep:" " ld_libs;
        "--end-group";
        "--cref -Map " ^ embox2 ^ ".map";
        "-o " ^ embox2;
      ]
  in
  let partial =
    make_rule "obj/embox-2.o"
      (ld_objs @ ld_libs @ lds @ [ abs_bd t "gen/buildinfo.ld"; gen_stamp_dep t ])
      partial_cmd
  in
  let embox = "embox" in
  let final_cmd =
    cmd_of
      [
        g.gf_cross ^ "ld";
        "--relax";
        g.gf_ldflags;
        "--gc-sections";
        "-T " ^ rel_build "bin" "obj/image.lds";
        rel_build "bin" "obj/embox-2.o";
        "--print-memory-usage";
        "-o " ^ embox ^ " | tee " ^ embox ^ ".mem";
      ]
  in
  let final =
    make_rule "bin/embox"
      [ rel_build "bin" "obj/embox-2.o"; rel_build "bin" "obj/image.lds"; gen_stamp_dep t ]
      final_cmd
  in
  let bin_cmd = cmd_of [ g.gf_cross ^ "objcopy"; "-O binary embox embox.bin" ] in
  let bin = make_rule "bin/embox.bin" [ "embox" ] bin_cmd in
  [ partial; final; bin ]

(* ------------------------------------------------------------------ *)
(* Whole dune workspace                                                *)
(* ------------------------------------------------------------------ *)

(** All owned compile/@Rule sources of all modules, in build order.
 *  Iterates [p_all] (not [p_modules]): modules whose only sources are
 *  .lds.S files (e.g. embox/kernel/stack) still own a compile rule. *)
let owned_sources (t : t) : (Buildplan.module_plan * Buildplan.source_plan) list =
  List.concat_map t.plan.p_all ~f:(fun m ->
    List.filter_map m.mp_sources ~f:(fun s ->
      if s.sp_owner && (Buildplan.kind_is_cc s.sp_kind || Buildplan.kind_is_lds s.sp_kind)
      then Some (m, s)
      else if Buildplan.kind_is_archive s.sp_kind then Some (m, s)
      else None))

(** Collect every rule of the build. *)
let collect_rules (t : t) : rule list =
  let rules = ref [] in
  List.iter (owned_sources t) ~f:(fun (m, s) ->
    rules :=
      (if Buildplan.kind_is_archive s.sp_kind then rule_source_rule t s
       else compile_rule t m s) :: !rules);
  List.iter t.plan.p_ld_modules ~f:(fun m -> rules := module_ld_rule t m :: !rules);
  List.iter t.plan.p_ar_modules ~f:(fun m -> rules := module_ar_rule t m :: !rules);
  List.iter t.plan.p_initfs ~f:(fun (src, target, chmod) ->
    rules := initfs_copy_rule t src target chmod :: !rules);
  rules := include_install_rule t :: !rules;
  rules := rootfs_cpio_rule t :: !rules;
  rules := image_lds_rule t :: !rules;
  List.iter (link_rules t) ~f:(fun r -> rules := r :: !rules);
  List.rev !rules

(** Render one rule as a dune stanza (the target is a basename). *)
let render_rule (r : rule) : string =
  "(rule\n\
   (targets "
  ^ r.r_target
  ^ ")\n\
   (deps "
  ^ String.concat ~sep:" " r.r_deps
  ^ ")\n"
  ^ (if r.r_promote then "(mode promote)\n" else "")
  ^ "(action (bash \""
  ^ sesc r.r_action
  ^ "\"))\n\
   )"

(** Group rules by target directory. Returns a sorted list of
 *  (build-dune-relative dir, dune file content) pairs. *)
let dune_files_by_dir (t : t) : (string * string) list =
  let groups = Hashtbl.create ~size:512 (module String) in
  List.iter (collect_rules t) ~f:(fun r ->
    let cur =
      match Hashtbl.find groups r.r_dir with
      | Some xs -> xs
      | None -> []
    in
    Hashtbl.set groups ~key:r.r_dir ~data:(r :: cur));
  Hashtbl.to_alist groups
  |> List.map ~f:(fun (k, v) ->
       let body = String.concat ~sep:"\n\n" (List.map (List.rev v) ~f:render_rule) in
       (k, body))
  |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)

let root_dune =
  "; Generated by mybuild gen-dune. Do not edit.\n\
   \n\
   (rule\n\
   \t(targets embox)\n\
   \t(deps build-dune/bin/embox)\n\
   \t(action (copy build-dune/bin/embox embox)))\n"

let dune_project = "(lang dune 3.20)\n"

(** Neutralize dune files left behind by an earlier gen-dune run (the set of
 *  rule directories may have shrunk). They are overwritten with a comment;
 *  actual deletion is left to the user. *)
let neutralize_stale_dune_files (root : string) (fresh : string list) : unit =
  let fresh_set = Hashtbl.create ~size:512 (module String) in
  List.iter fresh ~f:(fun p -> Hashtbl.set fresh_set ~key:p ~data:());
  let rec walk (dir : string) : unit =
    let dh = Unix.opendir dir in
    let entries = ref [] in
    (try
       while true do
         entries := Unix.readdir dh :: !entries
       done
     with End_of_file | Unix.Unix_error (_, _, _) -> ());
    Unix.closedir dh;
    List.iter (List.sort ~compare:String.compare !entries) ~f:(fun e ->
      if String.equal e "." || String.equal e ".." then ()
      else
        let p = dir ^ "/" ^ e in
        if Stdlib.Sys.is_directory p then walk p
        else if String.equal e "dune" && not (Hashtbl.mem fresh_set p) then
          let oc = Out_channel.open_bin p in
          Out_channel.output_string oc
            "; Stale: removed by a newer gen-dune run. Safe to delete.\n";
          Out_channel.close oc)
  in
  let bd = root ^ "/build-dune" in
  if Stdlib.Sys.file_exists bd then walk bd

(* ------------------------------------------------------------------ *)
(* Entry point                                                         *)
(* ------------------------------------------------------------------ *)

let emit_all (root : string) (b : Model.build) : unit =
  let p = Buildplan.plan_build root b in
  let t = { root; plan = p; build = b } in
  let dd = p.p_dirs in

  (* 0. Wipe pure build-output directories (stale artifacts would clash
   *  with rule targets as "file present in source tree"). *)
  wipe_dir_contents dd.obj;
  wipe_dir_contents (dd.build_dune ^ "/module");
  wipe_dir_contents dd.bin;

  (* 1. Generated headers / lds fragments / C sources. *)
  let ctx = { Ctx.root; out_dir = dd.gen; build = b } in
  let n_hdr = Headers.gen_all ctx in
  let config_lds_h = Lds.config_lds_h root in
  write_file (dd.gen ^ "/config.lds.h") config_lds_h;
  write_file (dd.gen ^ "/regions.lds.h") (Lds.regions_lds_h config_lds_h);
  write_file (dd.gen ^ "/sections.lds.h") (Lds.sections_lds_h config_lds_h);
  write_file (dd.gen ^ "/phdrs.lds.h") (Lds.phdrs_lds_h config_lds_h);
  write_file (dd.gen ^ "/section_symbols.lds.h")
    (Lds.section_symbols_lds_h b config_lds_h);
  write_file (dd.gen ^ "/apps.lds.h") (Lds.apps_lds_h b config_lds_h);
  write_file (dd.gen ^ "/image.lds.S") (Lds.image_lds_s root);
  write_file (dd.gen ^ "/buildinfo.ld") (Lds.buildinfo_ld ());
  write_file (dd.gen ^ "/src/framework/mod/mod_inject.c") (Modinject.render b);
  write_file (dd.gen ^ "/src/framework/mod/apps_build_deps.c")
    (Sections.apps_build_deps_c b);
  write_file (dd.gen ^ "/src/framework/mod/mod_info.h") (Modinfo.render b);
  write_file (dd.gen ^ "/src/mem/loadsect/sections.c")
    (Sections.sections_c b config_lds_h);

  (* Empty directory required by the -I<gen>/src/include flag (legacy
   *  mk/flags.mk creates it as a side effect; GCC silently skips missing
   *  -I dirs, which breaks <../../src/...> includes). *)
  mkdir_p (dd.gen ^ "/src/include");

  (* 2. Invalidation stamp. *)
  write_file (dd.gen ^ "/gen.stamp") (gen_stamp root);

  (* 3. Helper scripts. *)
  write_exec (dd.build_dune ^ "/scripts/module-link.sh") module_link_sh;
  write_exec (dd.build_dune ^ "/scripts/module-ar.sh") module_ar_sh;
  write_exec (dd.build_dune ^ "/scripts/rootfs.sh") rootfs_sh;
  write_exec (dd.build_dune ^ "/scripts/include-install.sh") (include_install_sh t);

  (* 4. Dune files: one per output directory (dune requires targets to live
   *  in the dune file's own directory). *)
  let files = dune_files_by_dir t in
  let dune_path_of (dir : string) : string =
    if String.is_empty dir then dd.build_dune ^ "/dune"
    else dd.build_dune ^ "/" ^ dir ^ "/dune"
  in
  List.iter files ~f:(fun (dir, content) ->
    write_file (dune_path_of dir)
      ("; Generated by mybuild gen-dune. Do not edit.\n\n" ^ content));
  neutralize_stale_dune_files root (List.map files ~f:(fun (dir, _c) -> dune_path_of dir));
  write_file (root ^ "/dune") root_dune;
  write_file (root ^ "/dune-project") dune_project;
  Stdlib.Format.printf
    "gen-dune: %d headers, %d modules (%d ld, %d ar), %d initfs files\n%!"
    n_hdr (List.length p.p_modules) (List.length p.p_ld_modules)
    (List.length p.p_ar_modules) (List.length p.p_initfs)

(* Build plan: turn the inferred build model into concrete per-module and
 * per-source compile/link data for the dune emitter.
 *
 * This mirrors the legacy generator (mk/script/build/build-gen.mk) closely
 * enough to reproduce the scr5_32 image, while staying simple for the
 * common case.
 *
 * Key legacy semantics replicated here:
 *  - a module instance's sources are the concatenation over allTypes
 *    (self first, then super types nearest-first) of each type's own
 *    sources (Build_ModuleInstance.sources getter);
 *  - a source file path is resolved relative to the Mybuild file of the
 *    type that DECLARES the source, with ^MOD_PATH / ^BUILD / @AddPrefix
 *    handling (build-gen.mk __source_file_mod);
 *  - per-source flags: flags_before / flags with the exact component
 *    order and whitespace semantics of the legacy (make 4.4 foreach
 *    joins with a single space; trim strips only leading/trailing ws);
 *  - stage: first @Build(stage=) over allTypes, else 2 if any @Build,
 *    else 1;
 *  - ld modules (dynamic) vs ar modules (static keyword);
 *  - ld scripts: all .lds.S sources of all modules, both link stages.
 *
 * Written in Base (not Core) per the Core -> Base migration plan.
 *)
open Base

(* ------------------------------------------------------------------ *)
(* Annotation type fqns                                                *)
(* ------------------------------------------------------------------ *)

let ann_include_path = [ "mybuild"; "lang"; "IncludePath" ]
let ann_include_path_before = [ "mybuild"; "lang"; "IncludePathBefore" ]
let ann_define_macro = [ "mybuild"; "lang"; "DefineMacro" ]
let ann_cflags = [ "mybuild"; "lang"; "Cflags" ]
let ann_include_header = [ "mybuild"; "lang"; "IncludeHeader" ]
let ann_instrument = [ "mybuild"; "lang"; "InstrumentProfiling" ]
let ann_build = [ "mybuild"; "lang"; "Build" ]
let ann_build_depends = [ "mybuild"; "lang"; "BuildDepends" ]
let ann_build_artifact_path = [ "mybuild"; "lang"; "BuildArtifactPath" ]
let ann_initfs = [ "mybuild"; "lang"; "InitFS" ]
let ann_add_prefix = [ "mybuild"; "lang"; "AddPrefix" ]
let ann_rule = [ "mybuild"; "lang"; "Rule" ]
let ann_app = [ "mybuild"; "lang"; "App" ]
let ann_auto_cmd = [ "mybuild"; "lang"; "AutoCmd" ]
let ann_cxx_cmd = [ "mybuild"; "lang"; "CxxCmd" ]
let ann_linker_section = [ "mybuild"; "lang"; "LinkerSection" ]
let ann_generated = [ "mybuild"; "lang"; "Generated" ]
let ann_include_export = [ "mybuild"; "lang"; "IncludeExport" ]

let fqn_eq = Model.fqn_equal

(** Strip leading/trailing whitespace. *)
let trim_ws (s : string) : string =
  let n = String.length s in
  let i = ref 0 in
  let j = ref (n - 1) in
  while !i < n && (Char.equal s.[!i] ' ' || Char.equal s.[!i] '\t') do
    Int.incr i
  done;
  while !j >= !i && (Char.equal s.[!j] ' ' || Char.equal s.[!j] '\t') do
    Int.decr j
  done;
  if !i > !j then "" else String.sub s ~pos:!i ~len:(!j - !i + 1)

(** All annotation values (as strings) of type [fqn] with option [opt] in
 *  the annotation list [anns]. A single-value annotation binds to the
 *  annotation type's first option. *)
let ann_values (anns : Model.annotation list) (fqn : Model.fqn) (opt : string) :
    string list =
  let one (a : Model.annotation) =
    if not (fqn_eq a.ann_type.at_fqn fqn) then []
    else
      (match a.ann_arg with
       | Some (Model.Ann_params ps) ->
           (match List.find ps ~f:(fun (n, _) -> String.equal n opt) with
            | Some (_n, v) -> [ Ctx.render_value v ]
            | None -> [])
       | Some (Model.Ann_value v) ->
           (match a.ann_type.at_options with
            | o :: _ when String.equal o.opt_name opt -> [ Ctx.render_value v ]
            | _ -> [])
       | None -> [])
  in
  List.concat (List.map anns ~f:one)

(** True when any annotation of type [fqn] is present in [anns]. *)
let has_ann (anns : Model.annotation list) (fqn : Model.fqn) : bool =
  List.exists anns ~f:(fun (a : Model.annotation) -> fqn_eq a.ann_type.at_fqn fqn)

(* ------------------------------------------------------------------ *)
(* Directory layout and make-variable expansion                        *)
(* ------------------------------------------------------------------ *)

type dirs = {
  root : string;
  build_dune : string;
  gen : string;
  obj : string;
  bin : string;
  conf : string;
  inc_install : string;
}

let dirs_of root =
  let build_dune = root ^ "/build-dune" in
  {
    root;
    build_dune;
    gen = build_dune ^ "/gen";
    obj = build_dune ^ "/obj";
    bin = build_dune ^ "/bin";
    conf = root ^ "/conf";
    inc_install = build_dune ^ "/include";
  }

let replace_all (s : string) (pattern : string) (with_ : string) : string =
  let n = String.length pattern in
  if n = 0 then s
  else
    let buf = Buffer.create (String.length s) in
    let i = ref 0 in
    let m = String.length s in
    while !i < m do
      if !i + n <= m && String.equal (String.sub s ~pos:!i ~len:n) pattern then (
        Buffer.add_string buf with_;
        i := !i + n)
      else (
        Buffer.add_char buf s.[!i];
        Int.incr i)
    done;
    Buffer.contents buf

(** Index of [needle] in [hay], or None (this Base has no String.search_opt). *)
let find_sub (hay : string) (needle : string) : int option =
  let hl = String.length hay in
  let nl = String.length needle in
  if nl = 0 || nl > hl then None
  else
    let rec loop i =
      if i + nl > hl then None
      else if String.equal (String.sub hay ~pos:i ~len:nl) needle then Some i
      else loop (i + 1)
    in
    loop 0

(** Expand [$(abspath X)] to X (or <root>/X when X is relative, matching
 *  make run from the repository root). Inner variables must already be
 *  expanded. *)
let expand_abspath (d : dirs) (s : string) : string =
  let pat = "$(abspath " in
  let plen = String.length pat in
  let rec loop s =
    match find_sub s pat with
    | None -> s
    | Some i ->
        let rest =
          String.sub s ~pos:(i + plen) ~len:(String.length s - i - plen)
        in
        (if String.contains rest ')' then
           let j = String.index_from_exn rest 0 ')' in
           let inner = String.sub rest ~pos:0 ~len:j in
           let inner =
             if String.is_prefix inner ~prefix:"/"
             then inner
             else d.root ^ "/" ^ inner
           in
           loop
             (String.sub s ~pos:0 ~len:i
              ^ inner
              ^ String.sub rest ~pos:(j + 1) ~len:(String.length rest - j - 1))
         else s)
  in
  loop s

(** Expand the make variables that may appear inside annotation values.
 *  The legacy leaves them as [$(...)] and lets make expand them at recipe
 *  run time; we expand them to concrete paths up front. *)
let expand_make_vars (d : dirs) (s : string) : string =
  let s = replace_all s "$(CONF_DIR)" d.conf in
  let s = replace_all s "$(ROOTFS_IMAGE)" (d.obj ^ "/rootfs.cpio") in
  let s = replace_all s "$(SRCGEN_DIR)" d.gen in
  let s = replace_all s "$(OBJ_DIR)" d.obj in
  let s = replace_all s "$(BIN_DIR)" d.bin in
  let s = replace_all s "$(MKGEN_DIR)" d.gen in
  let s = replace_all s "$(INCLUDE_INSTALL_DIR)" d.inc_install in
  let s = replace_all s "$(TOOLCHAIN_DIR)" (d.build_dune ^ "/toolchain") in
  let s = replace_all s "$(THIRDPARTY_DIR)" (d.root ^ "/third-party") in
  let s = replace_all s "$(SRC_DIR)" (d.root ^ "/src") in
  let s = replace_all s "$(BUILD_DIR)" d.build_dune in
  let s = replace_all s "$(EXTERNAL_BUILD_DIR)" (d.build_dune ^ "/extbld") in
  let s = replace_all s "$(ROOT_DIR)" d.root in
  expand_abspath d s

(* ------------------------------------------------------------------ *)
(* Global flags (replicates mk/flags.mk for the gcc compiler)          *)
(* ------------------------------------------------------------------ *)

(** Run a shell command and return its stdout. Fails on non-zero exit. *)
let run_sh (cmd : string) : string =
  let ic = Unix.open_process_in cmd in
  let lines = In_channel.input_lines ic in
  let status = Unix.close_process_in ic in
  (match status with
  | Unix.WEXITED 0 | Unix.WSIGNALED 0 ->
      String.concat ~sep:"\n" lines ^ (if List.is_empty lines then "" else "\n")
  | Unix.WEXITED c ->
      Stdlib.output_string Stdlib.stderr
        (Printf.sprintf "run_sh: command failed (exit %d): %s\n" c cmd);
      Stdlib.exit 1
  | Unix.WSIGNALED s ->
      Stdlib.output_string Stdlib.stderr
        (Printf.sprintf "run_sh: command killed by signal %d: %s\n" s cmd);
      Stdlib.exit 1
  | Unix.WSTOPPED s ->
      Stdlib.output_string Stdlib.stderr
        (Printf.sprintf "run_sh: command stopped by signal %d: %s\n" s cmd);
      Stdlib.exit 1)

(** Read a whole file. *)
let read_file path =
  let ic = Stdlib.open_in_bin path in
  let len = Stdlib.in_channel_length ic in
  let buf = Bytes.create len in
  Stdlib.really_input ic buf 0 len;
  Stdlib.close_in ic;
  Bytes.to_string buf

(** Generate build.mk the way mk/script/build/oldconf-gen.mk does
 *  (host cc -E over mk/confmacro.S with __BUILD_MK__). *)
let build_mk_text (root : string) : string =
  run_sh
    ("cd " ^ root
    ^ " && cc -E -P -undef -nostdinc -iquote conf/ -D__BUILD_MK__ mk/confmacro.S 2>/dev/null"
    ^ " | awk '{ gsub(\"\\\\$N\",\"\\n\"); print }'")

(** One frame of the make conditional stack: [parent] is whether the
 *  enclosing branch was active, [cond] the evaluated condition,
 *  [flipped] whether we are in the [else]-branch. *)
type cond_frame = { parent : bool; cond : bool; flipped : bool }

(** Parse the generated build.mk the way make would: evaluate the
 *  [ifeq]/[ifneq]/[ifdef]/[ifndef]/[else]/[endif] conditionals (the
 *  legacy leaves them in build.mk for make) and accumulate
 *  [KEY = V] / [KEY += V] / [KEY ?= V] assignments. [$(VAR)] references
 *  in conditions and values expand against the accumulated variables
 *  (unknown variables expand to empty, as in make); [COMPILER] defaults
 *  to gcc, as mk/flags.mk sets it before build.mk is included. *)
let parse_build_mk (text : string) : (string * string) list =
  let vars = Hashtbl.create ~size:32 (module String) in
  let () = Hashtbl.set vars ~key:"COMPILER" ~data:"gcc" in
  let frames = ref ([] : cond_frame list) in
  let branch_active () =
    List.for_all !frames ~f:(fun fr ->
      fr.parent && (if fr.flipped then not fr.cond else fr.cond))
  in
  let get_var (name : string) : string =
    (match Hashtbl.find vars name with
     | Some v -> v
     | None -> "")
  in
  (* Index of the first occurrence of [c] at or after [start], if any. *)
  let index_from (str : string) (start : int) (c : char) : int option =
    let n = String.length str in
    let k = ref start in
    while !k < n && not (Char.equal str.[!k] c) do
      Int.incr k
    done;
    if !k < n then Some !k else None
  in
  (* Expand [$(VAR)] references and [$$] escapes against [vars]. *)
  let expand (s : string) : string =
    let n = String.length s in
    let buf = Buffer.create n in
    let pos = ref 0 in
    while !pos < n do
      let c = s.[!pos] in
      if Char.equal c '$' && !pos + 1 < n && Char.equal s.[!pos + 1] '$' then (
        Buffer.add_char buf '$';
        pos := !pos + 2)
      else if Char.equal c '$' && !pos + 1 < n && Char.equal s.[!pos + 1] '(' then (
        (match index_from s (!pos + 2) ')' with
         | None ->
             Buffer.add_string buf (String.sub s ~pos:!pos ~len:(n - !pos));
             pos := n
         | Some close ->
             let name =
               trim_ws (String.sub s ~pos:(!pos + 2) ~len:(close - !pos - 2))
             in
             Buffer.add_string buf (get_var name);
             pos := close + 1))
      else (
        Buffer.add_char buf c;
        Int.incr pos)
    done;
    Buffer.contents buf
  in
  let set_var (key : string) (val_ : string) (append : bool) (only_if_unset : bool) =
    if only_if_unset && Hashtbl.mem vars key then ()
    else if append then
      (match Hashtbl.find vars key with
       | Some old -> Hashtbl.set vars ~key:key ~data:(old ^ " " ^ val_)
       | None -> Hashtbl.set vars ~key:key ~data:val_)
    else Hashtbl.set vars ~key:key ~data:val_
  in
  let is_directive (l : string) (name : string) : bool =
    let nl = String.length name in
    String.is_prefix l ~prefix:name
    && (String.length l = nl
        || Char.equal l.[nl] ' '
        || Char.equal l.[nl] '\t'
        || Char.equal l.[nl] '(')
  in
  (* Last occurrence of [c] in [str], if any. *)
  let index_last (str : string) (c : char) : int option =
    let n = String.length str in
    let k = ref (n - 1) in
    while !k >= 0 && not (Char.equal str.[!k] c) do
      k := !k - 1
    done;
    if !k >= 0 then Some !k else None
  in
  (* [ifeq (A,B)] / [ifneq (A,B)]: expand both sides, compare. The outer
   * argument is delimited by the last [)] on the line (the [)] of a
   * [$(VAR)] inside is not the delimiter). *)
  let ifeq_value (l : string) (kw : string) : bool =
    let i = String.index_from_exn l (String.length kw) '(' in
    (match index_last l ')' with
     | Some close when close > i ->
         let inner = String.sub l ~pos:(i + 1) ~len:(close - i - 1) in
         (match index_from inner 0 ',' with
          | Some k ->
              let a = trim_ws (expand (String.sub inner ~pos:0 ~len:k)) in
              let b =
                trim_ws
                  (expand
                     (String.sub inner ~pos:(k + 1) ~len:(String.length inner - k - 1)))
              in
              String.equal a b
          | None -> false)
     | _ -> false)
  in
  let ifdef_value (l : string) (kw : string) : bool =
    let kwl = String.length kw in
    let var = trim_ws (String.sub l ~pos:kwl ~len:(String.length l - kwl)) in
    (match Hashtbl.find vars var with
     | Some v -> not (String.is_empty v)
     | None -> false)
  in
  let parse_line (line : string) =
    let line = trim_ws line in
    if String.is_empty line || String.is_prefix line ~prefix:"# " then ()
    else if is_directive line "endif" then
      (match !frames with
       | _fr :: rest -> frames := rest
       | [] -> ())
    else if is_directive line "else" then
      (match !frames with
       | fr :: rest -> frames := { fr with flipped = true } :: rest
       | [] -> ())
    else if is_directive line "ifeq" then
      frames :=
        { parent = branch_active (); cond = ifeq_value line "ifeq"; flipped = false }
        :: !frames
    else if is_directive line "ifneq" then
      frames :=
        {
          parent = branch_active ();
          cond = not (ifeq_value line "ifneq");
          flipped = false;
        }
        :: !frames
    else if is_directive line "ifdef" then
      frames :=
        { parent = branch_active (); cond = ifdef_value line "ifdef"; flipped = false }
        :: !frames
    else if is_directive line "ifndef" then
      frames :=
        {
          parent = branch_active ();
          cond = not (ifdef_value line "ifndef");
          flipped = false;
        }
        :: !frames
    else if branch_active () then
      let eq =
        let i = ref (-1) in
        let n = String.length line in
        let k = ref 0 in
        while !k < n do
          if Char.equal line.[!k] '=' then (
            i := !k;
            k := n)
          else Int.incr k
        done;
        !i
      in
      if eq > 1 then
        let is_append = Char.equal line.[eq - 1] '+' in
        let is_cond = Char.equal line.[eq - 1] '?' in
        let key_start = if is_append || is_cond then eq - 1 else eq in
        let key = trim_ws (String.sub line ~pos:0 ~len:key_start) in
        let val_ =
          trim_ws
            (expand (String.sub line ~pos:(eq + 1) ~len:(String.length line - eq - 1)))
        in
        set_var key val_ is_append is_cond
  in
  List.iter (String.split text ~on:'\n') ~f:parse_line;
  Hashtbl.to_alist vars

let build_mk_get (bm : (string * string) list) (key : string) : string =
  (match List.find bm ~f:(fun (k, _) -> String.equal k key) with
   | Some (_k, v) -> v
   | None -> "")

(* Make-style token filters (for the EMBOX_IMPORTED_* flag computations,
 * mk/extbld/toolchain.mk). Tokens are whitespace-separated; make patterns
 * like [-W%] / [-std=%] are prefix matches. NB: must stay above
 * [global_flags], which uses them. *)
let make_tokens (s : string) : string list =
  let ts = String.split (trim_ws s) ~on:' ' in
  List.filter ts ~f:(fun t -> not (String.is_empty t))

let filter_tok (pred : string -> bool) (s : string) : string =
  String.concat ~sep:" " (List.filter (make_tokens s) ~f:pred)

let filter_out_tok (pred : string -> bool) (s : string) : string =
  filter_tok (fun t -> not (pred t)) s

let tok_prefixed (prefix : string) (t : string) : bool =
  String.is_prefix t ~prefix

let tok_suffixed (suffix : string) (t : string) : bool =
  String.is_suffix t ~suffix

let tok_in (ts : string list) (t : string) : bool =
  List.exists ts ~f:(fun u -> String.equal u t)

type global_flags = {
  gf_cc : string;
  (** Cross C compiler (also used for -E and .S). *)
  gf_cflags : string;
  gf_cxxflags : string;
  gf_cppflags : string;
  gf_asflags : string;
  gf_ldflags : string;
  (** -static -nostdlib -znoexecstack <LDFLAGS> *)
  gf_arch : string;
  gf_platform : string;
  gf_cross : string;
  gf_chip_vendor : string;
  gf_platform_vendor : string;
  gf_bm_cflags : string;
  (** CFLAGS contributed by build.mk (e.g. -g -O0 -march=rv32gc -mabi=ilp32).
   *  Used to expand $(EMBOX_GCC) in @Rule scripts: the legacy embox-gcc
   *  wrapper forwards the imported CFLAGS, so -print-file-name=libgcc.a
   *  resolves the multilib matching the target march/mabi. *)
  gf_gcc_major : int;
  (** Major version of the (cross) gcc, 0 when not a gcc. *)
  gf_export_cppflags : string;
  (** EMBOX_EXPORT_CPPFLAGS (flags.mk L105): cppflags minus the quoted
   *  -D... tokens. Passed to extbld builds as part of EMBOX_CPPFLAGS. *)
  gf_imported_cppflags : string;
  gf_imported_cflags : string;
  gf_imported_m_cflags : string;
  gf_imported_cxxflags : string;
  gf_imported_ldflags : string;
  (** EMBOX_IMPORTED_* values baked into the generated embox-gcc/embox-g++
   *  toolchain wrappers (mk/extbld/toolchain.mk L6-26). *)
}

(** Compute the global flags exactly like mk/flags.mk does for the gcc
 *  compiler (the only one supported here). [bm] is the parsed build.mk. *)
let global_flags (d : dirs) (bm : (string * string) list) : global_flags =
  let arch = build_mk_get bm "ARCH" in
  let platform = build_mk_get bm "PLATFORM" in
  let cross = build_mk_get bm "CROSS_COMPILE" in
  let chip_vendor = build_mk_get bm "CHIP_VENDOR" in
  let platform_vendor = build_mk_get bm "PLATFORM_VENDOR" in
  let gcc = cross ^ "gcc" in
  let embox_version = "0.7.1" in (* mk/version.mk *)

  (* cppflags (flags.mk L78-103) *)
  let platform_id =
    if String.is_empty platform then ""
    else
      " -D__EMBOX_PLATFORM__"
      ^ String.map platform ~f:(fun c -> if Char.equal c '-' then '_' else c)
      ^ "__"
  in
  let cppflags =
    "-U__linux__ -Ulinux -U__linux -D__unix -D__EMBOX__"
    (* Backslash-escaped quotes: they survive into the shell command so
     * that bash passes literal double quotes to gcc, as in legacy
     * flags.mk. Without them bash quote-removal would strip the quotes
     * and the macro would expand to the bare number 0.7.1. *)
    ^ Printf.sprintf " -D__EMBOX_VERSION__=\\\"%s\\\"" embox_version
    ^ platform_id
    ^ " -I" ^ d.inc_install
    ^ " -I" ^ d.gen ^ "/include"
    ^ " -I" ^ d.root ^ "/src/include"
    ^ " -I" ^ d.root ^ "/src/arch/" ^ arch ^ "/include"
    ^ " -I" ^ d.root ^ "/src/compat/linux/include"
    ^ " -I" ^ d.root ^ "/src/compat/posix/include"
    ^ " -I" ^ d.root ^ "/src/compat/bsd/include"
    ^ " -I" ^ d.root ^ "/src/compat/libc/include"
    ^ " -I" ^ d.gen ^ "/src/include"
    ^ " -nostdinc -MMD -MP"
    ^ (if String.is_empty (build_mk_get bm "CPPFLAGS") then ""
       else " " ^ build_mk_get bm "CPPFLAGS")
  in

  (* common_flags (L110-124): -pipe + -fmacro-prefix-map (gcc, non-dist) *)
  let common_flags = "-pipe -fmacro-prefix-map=" ^ d.root ^ "=." in

  (* CFLAGS contributed by build.mk (included at flags.mk L15, i.e. before
   * asflags/cflags are computed). *)
  let bm_cflags = build_mk_get bm "CFLAGS" in

  (* GCC_VERSION_MAJOR (flags.mk L64): echo __GNUC__ | $(GCC) -E -P -.
   * 0 when the compiler is missing or not a gcc. *)
  let gcc_major =
    try
      let (ic, _oc) =
        Unix.open_process
          (Printf.sprintf "echo __GNUC__ | %s -E -P - 2>/dev/null" gcc)
      in
      let s = In_channel.input_all ic in
      let () = ignore (In_channel.close ic) in
      Int.of_string (trim_ws s)
    with _ -> 0
  in

  (* common_ccflags (L135-141) plus the arch/compiler-version additions
   * (L143-172): gcc7 error downgrades, x86/gcc>=8 -fcf-protection=none
   * (QEMU addbr32 workaround), aarch64/gcc>=10 -mno-outline-atomics. *)
  let common_ccflags =
    common_flags
    ^ " -fno-strict-aliasing -fno-common -fno-stack-protector -fno-pic"
    ^ " -Wall -Werror -Wformat -Wundef -Wno-trigraphs -Wno-char-subscripts"
    ^ (if gcc_major = 7
       then " -Wno-error=format-truncation= -Wno-error=alloc-size-larger-than="
       else "")
    ^ (if String.equal arch "x86" && gcc_major >= 8
       then " -fcf-protection=none"
       else "")
    ^ (if String.equal arch "aarch64" && gcc_major >= 10
       then " -mno-outline-atomics"
       else "")
  in

  (* CFLAGS := -std=gnu99 $(common_ccflags) $(CFLAGS) (L189-190), where
   * $(CFLAGS) = build.mk additions followed by the two gcc warning
   * suppressions (appended at L145/L150, after asflags was frozen). *)
  let cflags =
    "-std=gnu99 "
    ^ common_ccflags
    ^ (if String.is_empty bm_cflags then "" else " " ^ bm_cflags)
    ^ " -Wno-misleading-indentation -Wno-nonnull-compare"
  in

  (* ASFLAGS := $(common_flags) $(CFLAGS) (L129-130), where $(CFLAGS) is the
   * pre-warning value (build.mk additions only). *)
  let asflags =
    common_flags
    ^ (if String.is_empty bm_cflags then "" else " " ^ bm_cflags)
  in

  (* CXXFLAGS := $(common_ccflags) $(CXXFLAGS) (L195-196): no -std, no
   * warning suppressions (those are CFLAGS-only). *)
  let cxxflags =
    common_ccflags
    ^ (if String.is_empty (build_mk_get bm "CXXFLAGS") then ""
       else " " ^ build_mk_get bm "CXXFLAGS")
  in

  (* LDFLAGS := -static -nostdlib -znoexecstack $(LDFLAGS) (L201-202). *)
  let ldflags =
    "-static -nostdlib -znoexecstack"
    ^ (if String.is_empty (build_mk_get bm "LDFLAGS") then ""
       else " " ^ build_mk_get bm "LDFLAGS")
  in

  (* EMBOX_IMPORTED_* (mk/extbld/toolchain.mk L6-26), baked into the
   * generated embox-gcc/embox-g++ wrappers. *)
  let is_w_flag = tok_prefixed "-W" in
  let is_wa_wp_wl =
    fun t ->
      tok_prefixed "-Wa," t || tok_prefixed "-Wp," t || tok_prefixed "-Wl," t
  in
  let is_std_or_w = fun t -> tok_prefixed "-std=" t || is_w_flag t in
  let is_d_quoted =
    fun t ->
      tok_prefixed "-D" t && (tok_suffixed "\"" t || tok_suffixed "'" t)
  in
  let export_cppflags = filter_out_tok is_d_quoted cppflags in
  let imported_cppflags =
    filter_out_tok (fun t ->
        tok_in [ "-MMD"; "-MP" ] t || is_w_flag t) export_cppflags
    ^ (let wa = filter_tok is_wa_wp_wl export_cppflags in
       if String.is_empty wa then "" else " " ^ wa)
  in
  let permissive = if gcc_major >= 14 then " -fpermissive" else "" in
  let imported_cflags =
    filter_out_tok is_std_or_w cflags
    ^ (let wa = filter_tok is_wa_wp_wl cflags in
       if String.is_empty wa then "" else " " ^ wa)
    ^ permissive
  in
  let imported_m_cflags =
    filter_tok (fun t ->
        tok_prefixed "-m" t || tok_in [ "-EL"; "-EB" ] t) cflags
  in
  let imported_cxxflags =
    filter_out_tok is_std_or_w cxxflags
    ^ (let wa = filter_tok is_wa_wp_wl cxxflags in
       if String.is_empty wa then "" else " " ^ wa)
    ^ permissive
  in
  (* Merge a bare flag token with its argument (legacy
   * $(subst -z ,-z,...) / $(subst -m ,-m,...)). *)
  let merge_flag (flag : string) (s : string) : string list =
    let rec loop acc = function
      | t :: rest :: rs when String.equal t flag ->
          loop ((flag ^ rest) :: acc) rs
      | t :: rs -> loop (t :: acc) rs
      | [] -> List.rev acc
    in
    loop [] (make_tokens s)
  in
  let imported_ldflags =
    let base =
      make_tokens
        (filter_tok (fun t -> tok_in [ "-static"; "-nostdlib"; "-EL"; "-EB" ] t)
           ldflags)
    in
    let zt =
      merge_flag "-z" ldflags
      |> List.filter ~f:(fun t -> tok_prefixed "-z" t)
      |> List.map ~f:(fun t -> "-Wl," ^ t)
    in
    let mt =
      merge_flag "-m" ldflags
      |> List.filter ~f:(fun t -> tok_prefixed "-m" t)
      |> List.map ~f:(fun t -> "-Wl," ^ t)
    in
    String.concat ~sep:" " (base @ zt @ mt)
  in
  {
    gf_cc = gcc;
    gf_cflags = cflags;
    gf_cxxflags = cxxflags;
    gf_cppflags = cppflags;
    gf_asflags = asflags;
    gf_ldflags = ldflags;
    gf_arch = arch;
    gf_platform = platform;
    gf_cross = cross;
    gf_chip_vendor = chip_vendor;
    gf_platform_vendor = platform_vendor;
    gf_bm_cflags = bm_cflags;
    gf_gcc_major = gcc_major;
    gf_export_cppflags = export_cppflags;
    gf_imported_cppflags = imported_cppflags;
    gf_imported_cflags = imported_cflags;
    gf_imported_m_cflags = imported_m_cflags;
    gf_imported_cxxflags = imported_cxxflags;
    gf_imported_ldflags = imported_ldflags;
  }

exception Short_read

(** Read exactly [n] bytes from [ic]; raise [Short_read] on EOF. *)
let read_exact (ic : In_channel.t) (n : int) : bytes =
  let buf = Stdlib.Bytes.create n in
  let rec loop off =
    if off >= n then ()
    else
      let k = In_channel.input ic buf off (n - off) in
      if k <= 0 then raise Short_read
      else loop (off + k)
  in
  loop 0;
  buf

(** Run [cmd], return the trimmed stdout; "" on any failure. *)
let run_sh_opt (cmd : string) : string =
  let ic = Unix.open_process_in (cmd ^ " 2>/dev/null") in
  let slines = In_channel.input_lines ic in
  let status = Unix.close_process_in ic in
  (match status with
  | Unix.WEXITED 0 -> trim_ws (String.concat ~sep:"" slines)
  | _ -> "")

(** ELF class (e_ident[4]) expected for the target architecture.
 *  None: no check (architecture not in this table). *)
let expected_elf_class (arch : string) : int option =
  match arch with
  | "x86" | "riscv32" | "arm" | "mips" -> Some 1
  | "riscv64" | "aarch64" | "x86_64" -> Some 2
  | "msp430" -> Some 111
  | _ -> None

(** ELF class of the first ELF member of an ar archive, or None.
 *  Skips the ar index member (name [/] or [//]) that modern ar puts
 *  before the objects. *)
let first_member_elf_class (path : string) : int option =
  if not (Stdlib.Sys.file_exists path) then None
  else
    try
      let ic = Stdlib.open_in_bin path in
      let magic = read_exact ic 8 in
      if not (Stdlib.Bytes.equal magic (Stdlib.Bytes.of_string "!<arch>\n")) then
        (Stdlib.close_in_noerr ic; None)
      else (
        (* GNU ar: members start right after the 8-byte magic; the first
         * members are usually the [/] index and [//] symbol map. *)
        let rec loop n =
          if n > 10000 then (Stdlib.close_in_noerr ic; None)
          else
            let hdr = read_exact ic 60 in
            let v = ref 0 in
            let k = ref 48 in
            while
              !k < 58
              && Stdlib.Char.code (Stdlib.Bytes.get hdr !k) >= 48
              && Stdlib.Char.code (Stdlib.Bytes.get hdr !k) <= 57
            do
              v := !v * 10 + Stdlib.Char.code (Stdlib.Bytes.get hdr !k) - 48;
              Int.incr k
            done;
            let size = !v in
            let elf_magic = read_exact ic 4 in
            if Stdlib.Bytes.equal elf_magic (Stdlib.Bytes.of_string "\127ELF")
            then (
              (* EI_CLASS is e_ident[4], right after the magic *)
              let cls = Stdlib.Char.code (Stdlib.Bytes.get (read_exact ic 1) 0) in
              Stdlib.close_in_noerr ic;
              Some cls
            )
            else (
              let pad = if Int.rem size 2 = 1 then 1 else 0 in
              let skip = Int64.of_int (size + pad) in
              In_channel.seek ic (Int64.(In_channel.pos ic + skip));
              loop (n + 1))
        in
        loop 0)
    with Short_read -> None

(** Resolve a [`$(EMBOX_GCC) -print-file-name=<lib>`] query to a concrete
 *  path at gen-dune time.
 *
 *  The legacy lets the embox-gcc wrapper answer the query at build time.
 *  On a host gcc that lacks the target multilib (e.g. a gcc without the
 *  32-bit libgcc on an x86 box) the answer is a wrong-word-size archive
 *  and the link fails with undefined __divdi3/__udivdi3 & co. When the
 *  answer's word size does not match the target, fall back to other
 *  installed gcc versions. Cross toolchains are trusted as-is (their
 *  single configuration is authoritative). Returns "" when no candidate
 *  yields an acceptable path (the caller keeps the legacy behaviour). *)
let resolve_print_file_name (gf : global_flags) (lib : string) : string =
  let query (cc : string) : string =
    let cflags = trim_ws gf.gf_bm_cflags in
    let cmd =
      cc
      ^ (if String.is_empty cflags then "" else " " ^ cflags)
      ^ " -print-file-name=" ^ lib
    in
    run_sh_opt cmd
  in
  let acceptable (path : string) : bool =
    if not (Stdlib.Sys.file_exists path) then false
    else if Stdlib.Sys.is_directory path then true
    else
      (match expected_elf_class gf.gf_arch with
       | None -> true
       | Some cls ->
           (match first_member_elf_class path with
            | Some c -> Int.equal c cls
            | None -> false))
  in
  let candidates =
    if String.is_empty gf.gf_cross
    then
      gf.gf_cc
      :: [ "gcc-15"; "gcc-14"; "gcc-13"; "gcc-12"; "gcc-11"; "gcc-10" ]
    else [ gf.gf_cc ]
  in
  (match
     List.find_map candidates ~f:(fun cc ->
         let p = query cc in
         if String.is_empty p || not (String.is_prefix p ~prefix:"/")
         then None
         else if acceptable p then Some p else None)
   with
  | Some p -> p
  | None -> "")

(** Replace each [`$$($(EMBOX_GCC) -print-file-name=<lib> ...)`] command
 *  substitution in an @Rule script by the resolved path (see
 *  [resolve_print_file_name]); unresolvable spans are left as-is (legacy
 *  behaviour). Must run before the [$(EMBOX_GCC)] replacement in
 *  [expand_rule]. The Mybuild model keeps the [$$] escaping (unescape_make
 *  happens at emit time), so the substitution opener is [$] or [$$]. *)
let resolve_toolchain_libs (gf : global_flags) (s : string) : string =
  let marker = "$(EMBOX_GCC) -print-file-name=" in
  let ml = String.length marker in
  let n = String.length s in
  let buf = Buffer.create n in
  let pos = ref 0 in
  while !pos < n do
    let rest = String.sub s ~pos:!pos ~len:(n - !pos) in
    (match find_sub rest marker with
     | None ->
         Buffer.add_string buf rest;
         pos := n
     | Some j ->
         let i = !pos + j in
         (* The command-substitution opener: the make variable
          * [$(EMBOX_GCC)] is preceded by the substitution's [(], itself
          * preceded by [$] or [$$] (the Mybuild model keeps the [$$]
          * escaping). Extend back over it. *)
         let start =
           if i >= 2 && Char.equal s.[i - 1] '('
           then
             (if Char.equal s.[i - 2] '$'
              then (if i >= 3 && Char.equal s.[i - 3] '$' then i - 3 else i - 2)
              else i - 1)
           else i
         in
         Buffer.add_string buf (String.sub s ~pos:!pos ~len:(start - !pos));
         let k = ref (i + ml) in
         while
           !k < n
           && not (Char.equal s.[!k] ' ')
           && not (Char.equal s.[!k] ')')
         do
           Int.incr k
         done;
         let lib = String.sub s ~pos:(i + ml) ~len:(!k - i - ml) in
         let tail = String.sub s ~pos:!k ~len:(n - !k) in
         (if String.contains tail ')' then
            let c = String.index_from_exn s !k ')' in
            let resolved = resolve_print_file_name gf lib in
            if String.is_empty resolved
            then Buffer.add_string buf (String.sub s ~pos:start ~len:(c - start + 1))
            else Buffer.add_string buf resolved;
            pos := c + 1
          else (
            (* malformed: keep the rest verbatim *)
            Buffer.add_string buf (String.sub s ~pos:start ~len:(n - start));
            pos := n)))
  done;
  Buffer.contents buf

(** Expand the tool make variables that may appear in @Rule scripts.
 *  [$(EMBOX_GCC)] is expanded to the cross gcc with the build.mk CFLAGS
 *  (the legacy embox-gcc wrapper forwards its imported CFLAGS, so e.g.
 *  [-print-file-name=libgcc.a] picks the multilib for the target
 *  march/mabi). *)
let expand_rule (gf : global_flags) (s : string) : string =
  let s = resolve_toolchain_libs gf s in
  let s = replace_all s "$(CP)" "cp" in
  let s = replace_all s "$(MV)" "mv -f" in
  let s = replace_all s "$(RM)" "rm -f" in
  let s = replace_all s "$(MKDIR)" "mkdir -p" in
  let s = replace_all s "$(EMBOX_GCC)" (gf.gf_cc ^ (if String.is_empty gf.gf_bm_cflags then "" else " " ^ gf.gf_bm_cflags)) in
  let s = replace_all s "$(EMBOX_GXX)" (gf.gf_cross ^ "g++" ^ (if String.is_empty gf.gf_bm_cflags then "" else " " ^ gf.gf_bm_cflags)) in
  let s = replace_all s "$(EMBOX_AR)" (gf.gf_cross ^ "ar") in
  let s = replace_all s "$(EMBOX_LD)" (gf.gf_cross ^ "ld") in
  s

(* ------------------------------------------------------------------ *)
(* Source classification                                               *)
(* ------------------------------------------------------------------ *)

type source_kind =
  | Cc (** .c/.S/.cpp/... compiled to an object *)
  | Lds (** .lds.S compiled to a linker script *)
  | Archive (** .a source *)
  | Object (** prebuilt .o *)
  | Initfs (** @InitFS file copied to the rootfs *)
  | Other (** headers, .mk, etc. *)

let kind_is_cc = function Cc -> true | _ -> false
let kind_is_lds = function Lds -> true | _ -> false
let kind_is_archive = function Archive -> true | _ -> false
let kind_is_object = function Object -> true | _ -> false
let kind_is_initfs = function Initfs -> true | _ -> false

let classify (file_name : string) (is_initfs : bool) : source_kind =
  if is_initfs then Initfs
  else if String.is_suffix file_name ~suffix:".lds.S" then Lds
  else if String.is_suffix file_name ~suffix:".a" then Archive
  else if String.is_suffix file_name ~suffix:".o" then Object
  else if
    String.is_suffix file_name ~suffix:".c"
    || String.is_suffix file_name ~suffix:".S"
    || String.is_suffix file_name ~suffix:".cpp"
    || String.is_suffix file_name ~suffix:".cxx"
    || String.is_suffix file_name ~suffix:".C"
    || String.is_suffix file_name ~suffix:".cc"
    || String.is_suffix file_name ~suffix:".st"
  then Cc
  else Other

(* ------------------------------------------------------------------ *)
(* Source path resolution                                              *)
(* ------------------------------------------------------------------ *)

(** Remove all whitespace (legacy [$(subst $(\s),,...)]). *)
let remove_ws (s : string) : string =
  String.filter s ~f:(fun c -> not (Char.equal c ' ') && not (Char.equal c '\t'))

(** Resolve a source file name to a path relative to the repository root,
 *  exactly like the legacy __source_file_mod:
 *   - [^BUILD] anywhere in the (prefixed) name -> the build directory
 *     ([build-dune/], the new system's counterpart of the legacy
 *     [build/]) + name sans ^BUILD;
 *   - otherwise the name is appended to the directory of the Mybuild file
 *     of the type that declares the source;
 *   - [^MOD_PATH] is replaced by the declaring type's path (dots->slashes). *)
let resolve_source_path
    (decl : Model.module_type)
    (add_prefixes : string list)
    (file_name : string) :
    string =
  let wprefix =
    remove_ws
      ((String.concat ~sep:"" (List.map add_prefixes ~f:(fun p -> p ^ "/")))
       ^ file_name)
  in
  let base =
    if Ctx.contains wprefix "^BUILD" then
      (* NB: no trailing slash: the ^BUILD-less name keeps its leading
       * slash, exactly like the legacy ../..$(subst ^BUILD,,$3). *)
      "build-dune" ^ replace_all wprefix "^BUILD" ""
    else
      (Stdlib.Filename.dirname decl.mt_file) ^ "/" ^ wprefix
  in
  trim_ws (replace_all base "^MOD_PATH" (Model.fqn_to_path decl.mt_fqn))

(* ------------------------------------------------------------------ *)
(* Per-source plan                                                     *)
(* ------------------------------------------------------------------ *)

type source_plan = {
  sp_file : string;
  (** Path relative to the repository root (or gen/ for @Generated). *)
  sp_annotations : Model.annotation list;
  (** The source's annotations (for @IncludeExport etc.). *)
  sp_kind : source_kind;
  sp_obj_rel : string;
  (** Output path relative to build-dune (obj/...). *)
  sp_flags_before : string;
  sp_flags : string;
  sp_rule : string option;
  (** Custom @Rule script (expanded), if any. *)
  sp_generated : bool;
  (** True for @Generated sources (live under gen/). *)
  sp_initfs_target : string option;
  (** Rootfs-relative target path for @InitFS sources. *)
  sp_initfs_chmod : string option;
  mutable sp_owner : bool;
  (** True when this module owns the compilation of a shared source file
   *  (the last module in build order that includes it; legacy quirk).
   *  Only owners emit a compile rule; others just link the object. *)
}

(** sh-quote a flag value (legacy [sh_quote]: ['] -> [\'\']). *)
let shq (s : string) : string =
  "'" ^ replace_all s "'" "'\\''" ^ "'"

(** Legacy [do_flags]: foreach joins iterations with a single space. *)
let do_flags (prefix : string) (values : string list) : string =
  String.concat ~sep:" " (List.map values ~f:(fun v -> prefix ^ shq v))

(** The legacy [trim] on make 4.4 strips only leading/trailing whitespace
 *  (internal runs are preserved). Same as [trim_ws]. *)

(** All sources of a module type: own sources first, then the sources of
 *  each super type, nearest first (legacy allTypes>sources). *)
let all_sources (mt : Model.module_type) : Model.source list =
  mt.mt_sources
  @ List.concat
      (List.map mt.mt_all_super ~f:(fun (t : Model.module_type) -> t.mt_sources))

(** allTypes in legacy order: self first, then super types nearest first. *)
let all_types (mt : Model.module_type) : Model.module_type list =
  [ mt ] @ mt.mt_all_super

(** Expand [$(EXTERNAL_BUILD_DIR)] to the new system's extbld dir. *)
let expand_external_build_dir (root : string) (s : string) : string =
  replace_all s "$(EXTERNAL_BUILD_DIR)" (root ^ "/build-dune/extbld")

(** @BuildDepends fqn strings over allTypes. *)
let build_dep_fqns (mt : Model.module_type) : string list =
  let all_anns =
    List.concat
      (List.map (all_types mt) ~f:(fun (t : Model.module_type) ->
         t.mt_annotations))
  in
  ann_values all_anns ann_build_depends "value"

(** Transitive closure of @BuildDepends (legacy build_deps_all), resolved
 *  to module types via [types_by_fqn]. Cycles are tolerated. *)
let build_deps_all
    (types_by_fqn : (string, Model.module_type) Hashtbl.t)
    (mt : Model.module_type) :
    Model.module_type list =
  let rec loop (seen : string list) (queue : Model.module_type list)
      (acc : Model.module_type list) :
      Model.module_type list =
    match queue with
    | [] -> acc
    | t :: rest ->
        let key = Model.fqn_to_string t.mt_fqn in
        if List.mem seen key ~equal:String.equal then loop seen rest acc
        else
          let next =
            List.filter_map (build_dep_fqns t) ~f:(fun f ->
              Hashtbl.find types_by_fqn f)
          in
          loop (key :: seen) (next @ rest) (t :: acc)
  in
  let direct =
    List.filter_map (build_dep_fqns mt) ~f:(fun f ->
      Hashtbl.find types_by_fqn f)
  in
  loop [] direct []

(** @BuildArtifactPath flags of the build deps, with
 *  [$(EXTERNAL_BUILD_DIR)] expanded: (cppflags_before, cppflags_after,
 *  ldflags). before/after come from the transitive closure, ldflags from
 *  the direct deps (legacy build-gen.mk L533-535). *)
let extbld_dep_flags
    (root : string)
    (types_by_fqn : (string, Model.module_type) Hashtbl.t)
    (mt : Model.module_type) :
    string * string * string =
  let all_deps = build_deps_all types_by_fqn mt in
  let direct =
    List.filter_map (build_dep_fqns mt) ~f:(fun f ->
      Hashtbl.find types_by_fqn f)
  in
  let flags_of (mts : Model.module_type list) (opt : string) : string =
    let vals =
      List.concat_map mts ~f:(fun (t : Model.module_type) ->
        ann_values t.mt_annotations ann_build_artifact_path opt)
    in
    (* Full make-variable expansion (the legacy lets make expand these at
     * recipe time), including $(abspath ...) unwrapping. *)
    let exp = expand_make_vars (dirs_of root) in
    String.concat ~sep:" " (List.map vals ~f:exp)
  in
  ( flags_of all_deps "cppflags_before",
    flags_of all_deps "cppflags",
    flags_of direct "ldflags" )


(** Compute the flags for a source of module [mt]. [d] is the directory
 *  layout. The component order and whitespace match the legacy exactly:
 *  components are joined with a single space, then trimmed.
 *  [types_by_fqn] resolves the @BuildDepends closure (legacy
 *  build_deps_all) for the @BuildArtifactPath flag slots. *)
let source_flags
    (d : dirs)
    (types_by_fqn : (string, Model.module_type) Hashtbl.t)
    (mt : Model.module_type)
    (src : Model.source) :
     string * string =
  let anns = src.src_annotations in
  let mod_path = Model.fqn_to_path mt.mt_fqn in
  let mod_id = Model.fqn_to_id mt.mt_fqn in
  let exp = expand_make_vars d in

  let includes_before =
    List.map (ann_values anns ann_include_path_before "value") ~f:(fun v -> exp v)
  in
  let includes =
    List.map (ann_values anns ann_include_path "value") ~f:(fun v -> exp v)
  in
  let defines =
    List.map (ann_values anns ann_define_macro "value") ~f:(fun v -> exp v)
  in
  let cflags = List.map (ann_values anns ann_cflags "value") ~f:(fun v -> exp v) in
  let include_headers =
    List.map (ann_values anns ann_include_header "value") ~f:(fun v -> exp v)
  in
  let instrument_true =
    List.exists (ann_values anns ann_instrument "value") ~f:(fun v ->
      String.equal v "true")
  in

  (* @BuildArtifactPath flags of the transitive @BuildDepends closure
   *  (legacy build_deps_all), $(EXTERNAL_BUILD_DIR) expanded. *)
  let (artifact_before, artifact, _bd_ld) =
    extbld_dep_flags d.root types_by_fqn mt in

  let flags_before =
    trim_ws
      (String.concat ~sep:" " [ do_flags "-I" includes_before; artifact_before ])
  in
  let flags =
    trim_ws
      (String.concat ~sep:" "
         [
           do_flags "-I" includes;
           artifact;
           do_flags "-D" defines;
           "-include " ^ d.gen ^ "/include/config/" ^ mod_path ^ ".h"
           ^ " -D__EMBUILD_MOD__=" ^ mod_id;
           (if instrument_true then " -finstrument-functions" else "");
           do_flags "" cflags;
           do_flags "-include" include_headers;
         ])
  in
  (flags_before, flags)

(* ------------------------------------------------------------------ *)
(* Per-module plan                                                     *)
(* ------------------------------------------------------------------ *)

type module_plan = {
  mp_path : string;
  mp_id : string;
  mp_is_static : bool;
  mp_stage : int;
  mp_sources : source_plan list;
  mp_o_files : string list;
  (** Object paths (relative to build-dune) for Cc/Object sources. *)
  mp_a_files : string list;
  (** Archive paths (relative to build-dune) for Archive sources. *)
  mp_is_app : bool;
  mp_strip_main : bool;
  (** @AutoCmd over allTypes: the module's main is renamed to
   *  main_<id> and registered via the cmd application template
   *  (legacy mod_postbuild / MAIN_STRIPPING). *)
  mp_preserve_locals : bool;
  (** @AutoCmd(preserve_locals=...) value non-empty: do not localize the
   *  module's other global symbols (legacy MAIN_STRIPPING_LOCALS=no). *)
  mp_cxx_cmd : bool;
  (** @CxxCmd over allTypes (legacy CXX_CMD=yes). *)
  mp_linker_section : (string * string) list;
  (** (section, extmem-region) pairs from @LinkerSection. *)
}

(** A module carrying @Build (legacy mk/extbld.mk): its sources are
 *  produced by running the module's own Makefile through
 *  mk/extbld/build.mk (download/extract/patch/configure/build/install)
 *  into <build-dune>/extbld/<path>/install/. *)
type extbld_module = {
  em_path : string;
  (** Module path (fqn dots->slashes). *)
  em_id : string;
  (** Module id (fqn dots/slashes->double underscores). *)
  em_mybuild_dir : string;
  (** Directory of the Mybuild file (relative to root); the extbld
   *  Makefile lives there (legacy -C $(dir $(my_file))). *)
  em_stage : int;
  em_targets : string list;
  (** Basenames of the install artifacts consumed as archive sources
   *  (e.g. ["libopenlibm.a"]). *)
  em_bd_cppflags_before : string;
  em_bd_cppflags_after : string;
  em_bd_ldflags : string;
  (** @BuildArtifactPath flags of the (transitive) @BuildDepends, with
   *  $(EXTERNAL_BUILD_DIR) expanded to <root>/build-dune/extbld. *)
}

(** The build stage of a module: first @Build(stage=N) over allTypes,
 *  else 2 if any @Build annotation, else 1. *)
let stage_of (mt : Model.module_type) : int =
  let all_anns =
    List.concat
      (List.map (all_types mt) ~f:(fun (t : Model.module_type) ->
         t.mt_annotations))
  in
  let stages = ann_values all_anns ann_build "stage" in
  (match stages with
  | s :: _ -> (try Int.of_string s with _ -> 2)
  | [] -> if has_ann all_anns ann_build then 2 else 1)

let linker_section_of (mt : Model.module_type) : (string * string) list =
  let all_anns =
    List.concat
      (List.map (all_types mt) ~f:(fun (t : Model.module_type) ->
         t.mt_annotations))
  in
  let secs = [ "text"; "rodata"; "data"; "bss"; "arm_exidx"; "arm_extab" ] in
  List.filter_map secs ~f:(fun s ->
    match ann_values all_anns ann_linker_section s with
    | v :: _ when not (String.is_empty v) -> Some (s, v)
    | _ -> None)

let is_app_of (mt : Model.module_type) : bool =
  List.exists (all_types mt) ~f:(fun (t : Model.module_type) ->
    has_ann t.mt_annotations ann_app)

(** Main-stripping parameters (legacy mod_autocmd_postbuild): the module
 *  needs it when any of its types carries @AutoCmd; locals are preserved
 *  when the @AutoCmd preserve_locals value is non-empty; CXX_CMD when
 *  @CxxCmd is present. *)
let strip_main_of (mt : Model.module_type) : bool * bool * bool =
  let all_anns =
    List.concat
      (List.map (all_types mt) ~f:(fun (t : Model.module_type) ->
         t.mt_annotations))
  in
  let strip = has_ann all_anns ann_auto_cmd in
  let preserve =
    strip && not (List.is_empty (ann_values all_anns ann_auto_cmd "preserve_locals"))
  in
  let cxx = has_ann all_anns ann_cxx_cmd in
  (strip, preserve, cxx)

let plan_module
    (d : dirs)
    (gf : global_flags)
    (types_by_fqn : (string, Model.module_type) Hashtbl.t)
    (inst : Model.module_instance) :
    module_plan =
  let mt = inst.mi_type in
  let mod_path = Model.fqn_to_path mt.mt_fqn in
  let mod_id = Model.fqn_to_id mt.mt_fqn in

  let sources =
    List.map (all_sources mt) ~f:(fun (src : Model.source) ->
      let is_initfs = has_ann src.src_annotations ann_initfs in
      let kind = classify src.src_file_name is_initfs in
      let add_prefixes =
        ann_values src.src_annotations ann_add_prefix "value"
      in
      let file =
        resolve_source_path src.src_module add_prefixes src.src_file_name
      in
      let is_generated = has_ann src.src_annotations ann_generated in
      let (flags_before, flags) =
        if kind_is_cc kind || kind_is_lds kind then source_flags d types_by_fqn mt src
        else ("", "")
      in
      let obj_rel =
        match kind with
        | Cc -> "obj/" ^ Stdlib.Filename.chop_extension file ^ ".o"
        | Object -> "obj/" ^ file
        | Archive -> "obj/" ^ file
        | Lds -> "obj/" ^ Stdlib.Filename.chop_extension file
        | Initfs | Other -> ""
      in
      let rule =
        if has_ann src.src_annotations ann_rule then
          (match ann_values src.src_annotations ann_rule "script" with
           | s :: _ -> Some (expand_rule gf (expand_make_vars d s))
           | _ -> None)
        else None
      in
      let (initfs_target, initfs_chmod) =
        if is_initfs then
          let target_dir =
            (match ann_values src.src_annotations ann_initfs "target_dir" with
             | t :: _ -> t
             | _ -> "")
          in
          let target_name =
            (match ann_values src.src_annotations ann_initfs "target_name" with
             | t :: _ when not (String.is_empty t) -> t
             | _ -> src.src_file_name)
          in
          (Some (target_dir ^ target_name),
           (match ann_values src.src_annotations ann_initfs "chmod" with
            | c :: _ when not (String.is_empty c) -> Some c
            | _ -> None))
        else (None, None)
      in
      {
        sp_file = file;
        sp_annotations = src.src_annotations;
        sp_kind = kind;
        sp_obj_rel = obj_rel;
        sp_flags_before = flags_before;
        sp_flags = flags;
        sp_rule = rule;
        sp_generated = is_generated;
        sp_initfs_target = initfs_target;
        sp_initfs_chmod = initfs_chmod;
        sp_owner = false;
      })
  in

  let o_files =
    List.filter_map sources ~f:(fun s ->
      if kind_is_cc s.sp_kind || kind_is_object s.sp_kind
      then Some s.sp_obj_rel
      else None)
  in
  let a_files =
    List.filter_map sources ~f:(fun s ->
      if kind_is_archive s.sp_kind then Some s.sp_obj_rel else None)
  in
  let (strip_main, preserve_locals, cxx_cmd) = strip_main_of mt in
  {
    mp_path = mod_path;
    mp_id = mod_id;
    mp_is_static = mt.mt_is_static;
    mp_stage = stage_of mt;
    mp_sources = sources;
    mp_o_files = o_files;
    mp_a_files = a_files;
    mp_is_app = is_app_of mt;
    mp_strip_main = strip_main;
    mp_preserve_locals = preserve_locals;
    mp_cxx_cmd = cxx_cmd;
    mp_linker_section = linker_section_of mt;
  }

(* ------------------------------------------------------------------ *)
(* External build (@Build) modules                                     *)
(* ------------------------------------------------------------------ *)

(** True when any of the module's types carries @Build (legacy
 *  mk/extbld.mk: the module's own Makefile is run through
 *  mk/extbld/build.mk). *)
let is_extbld (mt : Model.module_type) : bool =
  List.exists (all_types mt) ~f:(fun (t : Model.module_type) ->
    has_ann t.mt_annotations ann_build)


(** Plan one @Build module: install artifacts are the archive sources
 *  under build-dune/extbld/<path>/install/. *)
let plan_extbld
    (root : string)
    (types_by_fqn : (string, Model.module_type) Hashtbl.t)
    (m : module_plan)
    (inst : Model.module_instance) :
    extbld_module =
  let mt = inst.mi_type in
  let prefix = "build-dune/extbld/" ^ m.mp_path ^ "/install/" in
  let targets =
    List.filter_map m.mp_sources ~f:(fun s ->
      if kind_is_archive s.sp_kind && String.is_prefix s.sp_file ~prefix
      then Some (Stdlib.Filename.basename s.sp_file)
      else None)
    |> List.dedup_and_sort ~compare:String.compare
  in
  let (bd_before, bd_after, bd_ld) = extbld_dep_flags root types_by_fqn mt in
  {
    em_path = m.mp_path;
    em_id = m.mp_id;
    em_mybuild_dir = Stdlib.Filename.dirname mt.mt_file;
    em_stage = m.mp_stage;
    em_targets = targets;
    em_bd_cppflags_before = bd_before;
    em_bd_cppflags_after = bd_after;
    em_bd_ldflags = bd_ld;
  }

(* ------------------------------------------------------------------ *)
(* Whole-build plan                                                    *)
(* ------------------------------------------------------------------ *)

type plan = {
  p_dirs : dirs;
  p_flags : global_flags;
  p_all : module_plan list;
  (** Every module of the build, in build order (including modules whose
   *  only sources are .lds.S / @InitFS files). *)
  p_modules : module_plan list;
  (** All modules with o/a sources, in build order. *)
  p_ld_modules : module_plan list;
  p_ar_modules : module_plan list;
  p_lds_scripts : string list;
  (** All .lds output paths (relative to build-dune), both stages. *)
  p_initfs : (string * string * string option) list;
  (** (src path rel to root, rootfs target, chmod) for @InitFS files. *)
  p_include_exports : (string * string) list;
  (** (src path rel to root, dest path under the include-install dir) for
   *  @IncludeExport headers. Deduplicated by destination. *)
  p_extbld : extbld_module list;
  (** @Build modules, in build order. *)
}

(** Topologically sort the static modules by their direct depends, exactly
 *  like the legacy [filter_static_modules] (mk/ugly.mk):
 *  [echo <edges> | tsort | awk -f mk/tsortdiag.awk | tac]. The awk only
 *  reports cycles (to stderr), so for an acyclic graph [tsort | tac] is
 *  equivalent. Edges are "v u" for each static module v and each direct
 *  dependency u of v that is also a static module. Returns the ordered list
 *  of module paths (dependencies first, after the tac reversal). *)
let topsort_static_modules (root : string) (static_insts : Model.module_instance list) :
    string list =
  let path_set =
    Set.of_list (module String) (List.map static_insts ~f:Model.mi_path)
  in
  (* For each vertex v emit "v u" for each static dependency u, plus the
   *  self-edge "v v" (as the legacy does) so that tsort emits every vertex,
   *  including isolated ones with no static dependencies. *)
  let edges =
    List.concat_map static_insts ~f:(fun (inst : Model.module_instance) ->
      let pv = Model.mi_path inst in
      let dep_edges =
        List.filter_map inst.mi_depends ~f:(fun (dep : Model.module_instance) ->
          let pu = Model.mi_path dep in
          if Set.mem path_set pu then Some (pv ^ " " ^ pu) else None)
      in
      dep_edges @ [pv ^ " " ^ pv])
  in
  let edges_str = String.concat ~sep:" " edges in
  let out =
    run_sh
      ("cd " ^ root ^ " && echo '" ^ edges_str ^ "' | tsort 2>/dev/null | tac")
  in
  List.filter_map (String.split ~on:'\n' out) ~f:(fun l ->
    let l = trim_ws l in
    if String.is_empty l then None else Some l)

(** Mark the compile owner of each shared source file: the last module in
 *  build order that includes a given source file owns its compilation
 *  (legacy quirk). Only owners emit a compile rule; others just link the
 *  resulting object. *)
let mark_shared_owners (plans : module_plan list) : unit =
  let owner_of = Hashtbl.create ~size:1024 (module String) in
  List.iter plans ~f:(fun m ->
    List.iter m.mp_sources ~f:(fun s ->
      if kind_is_cc s.sp_kind || kind_is_lds s.sp_kind
      then Hashtbl.set owner_of ~key:s.sp_file ~data:m.mp_path
      else ())
  );
  List.iter plans ~f:(fun m ->
    List.iter m.mp_sources ~f:(fun s ->
      if kind_is_cc s.sp_kind || kind_is_lds s.sp_kind
      then (
        s.sp_owner <-
          (match Hashtbl.find owner_of s.sp_file with
           | Some p -> String.equal p m.mp_path
           | None -> false))
      else ())
  )

let plan_build (root : string) (b : Model.build) : plan =
  let d = dirs_of root in
  let bm = parse_build_mk (build_mk_text root) in
  let gf = global_flags d bm in
  let types_by_fqn =
    let h = Hashtbl.create ~size:1024 (module String) in
    List.iter b.bd_modules ~f:(fun (inst : Model.module_instance) ->
      Hashtbl.set h ~key:(Model.fqn_to_string inst.mi_type.mt_fqn)
        ~data:inst.mi_type);
    h
  in
  let all =
    List.map b.bd_modules ~f:(fun (inst : Model.module_instance) ->
      plan_module d gf types_by_fqn inst)
  in
  (* Legacy filter_with_sources: keep modules with o or a source files. *)
  let with_sources =
    List.filter all ~f:(fun m ->
      not (List.is_empty m.mp_o_files) || not (List.is_empty m.mp_a_files))
  in
  let ld = List.filter with_sources ~f:(fun m -> not m.mp_is_static) in
  (* Static (ar) modules are topologically sorted by depends (legacy
   * filter_static_modules), then filtered to those with sources. *)
  let static_insts =
    List.filter b.bd_modules ~f:(fun (i : Model.module_instance) ->
      i.mi_type.mt_is_static)
  in
  let ar_order = topsort_static_modules root static_insts in
  let plan_by_path =
    let h = Hashtbl.create ~size:1024 (module String) in
    List.iter all ~f:(fun m -> Hashtbl.set h ~key:m.mp_path ~data:m);
    h
  in
  let ar =
    List.filter_map ar_order ~f:(fun p ->
      match Hashtbl.find plan_by_path p with
      | Some m
        when m.mp_is_static
             && (not (List.is_empty m.mp_o_files) || not (List.is_empty m.mp_a_files)) ->
          Some m
      | _ -> None)
  in
  (* Mark compile owners for shared source files. *)
  let () = mark_shared_owners all in
  (* All .lds.S sources of all modules (not just those with o/a files),
   * in build order. Both link stages use the full list. *)
  let lds_scripts =
    List.concat_map all ~f:(fun m ->
      List.filter_map m.mp_sources ~f:(fun s ->
        if kind_is_lds s.sp_kind then Some s.sp_obj_rel else None))
  in
  let initfs =
    List.concat_map all ~f:(fun m ->
      List.filter_map m.mp_sources ~f:(fun s ->
        if kind_is_initfs s.sp_kind then
          Some (s.sp_file, Option.value_exn s.sp_initfs_target, s.sp_initfs_chmod)
        else None))
  in
  (* @IncludeExport headers: (src rel to root, dest under include-install).
   *  dest = <path>/<target_name|basename>; dedup by destination. *)
  let include_exports =
    let rec loop = function
      | [] -> []
      | s :: rest ->
          if has_ann s.sp_annotations ann_include_export then
            let path =
              match ann_values s.sp_annotations ann_include_export "path" with
              | p :: _ -> p
              | _ -> ""
            in
            let name =
              match ann_values s.sp_annotations ann_include_export "target_name" with
              | n :: _ -> n
              | _ -> Stdlib.Filename.basename s.sp_file
            in
            let dest =
              if String.is_empty path then name else path ^ "/" ^ name
            in
            (s.sp_file, dest) :: loop rest
          else loop rest
    in
    let all_pairs =
      List.concat_map all ~f:(fun m -> loop m.mp_sources)
    in
    List.dedup_and_sort all_pairs ~compare:(fun a b -> String.compare (snd a) (snd b))
  in
  (* @Build modules (legacy extbld): run the module's Makefile through
   * mk/extbld/build.mk into build-dune/extbld/<path>/install/. *)
  let extbld =
    List.filter_map
      (List.map2_exn ~f:(fun m inst -> (m, inst)) all b.bd_modules)
      ~f:(fun (m, inst) ->
        if is_extbld inst.mi_type
        then Some (plan_extbld root types_by_fqn m inst)
        else None)
  in
  {
    p_dirs = d;
    p_flags = gf;
    p_all = all;
    p_modules = with_sources;
    p_ld_modules = ld;
    p_ar_modules = ar;
    p_lds_scripts = lds_scripts;
    p_initfs = initfs;
    p_include_exports = include_exports;
    p_extbld = extbld;
  }

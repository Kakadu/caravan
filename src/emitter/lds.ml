(* Generation of the linker-script fragment headers under gen/:
 *
 *   config.lds.h         (host cc -E over mk/confmacro.S)
 *   regions.lds.h        (mk/gen_ld_regions.sh semantics)
 *   sections.lds.h       (mk/gen_ld_sections.sh semantics)
 *   phdrs.lds.h          (mk/gen_ld_phdrs.sh semantics)
 *   section_symbols.lds.h(mk/script/user-lds-sections-symbols.mk)
 *   apps.lds.h           (mk/script/lds-apps.mk)
 *   buildinfo.ld         (mk/gen_buildinfo.sh)
 *   image.lds.S          (copy of mk/image.lds.S)
 *
 * config.lds.h is produced by shelling out to the host C preprocessor so
 * that the output is byte-identical to the legacy build; the rest are
 * reproduced in OCaml from the inferred build model.
 *
 * Written in Base (not Core) per the Core -> Base migration plan.
 *)
open Base

(* ------------------------------------------------------------------ *)
(* Small helpers                                                       *)
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

(** [sh_quote s] mirrors the legacy [sh_quote]: single-quote [s], escaping
 *  embedded single quotes as ['\''']. *)
let sh_quote s =
  "'" ^ String.map s ~f:(function '\'' -> '\\' | c -> c) ^ "'"

(* ------------------------------------------------------------------ *)
(* config.lds.h                                                        *)
(* ------------------------------------------------------------------ *)

(** [config_lds_h root] runs the host preprocessor over mk/confmacro.S
 *  exactly like mk/script/build/oldconf-gen.mk does, then applies the
 *  legacy awk transformation ($N -> newline, $ -> #). *)
let config_lds_h (root : string) : string =
  let cmd =
    "cd " ^ root
    ^ " && cc -E -P -undef -nostdinc -iquote conf/ -D__CONFIG_LDS_H__ mk/confmacro.S 2>/dev/null"
    ^ " | awk '{ gsub(\"\\\\$N\",\"\\n\"); gsub(\"\\\\$\",\"#\"); print }'"
  in
  run_sh cmd

(* ------------------------------------------------------------------ *)
(* regions / sections / phdrs                                          *)
(* ------------------------------------------------------------------ *)

(** All maximal runs of [prefix] followed by identifier characters, in
 *  order of appearance (mirrors [grep -o '<prefix>[0-9a-zA-Z_]*']). *)
let grep_o (prefix : string) (text : string) : string list =
  let n = String.length prefix in
  let m = String.length text in
  let is_ident c =
    Char.equal c '_'
    || (Char.compare c '0' >= 0 && Char.compare c '9' <= 0)
    || (Char.compare c 'a' >= 0 && Char.compare c 'z' <= 0)
    || (Char.compare c 'A' >= 0 && Char.compare c 'Z' <= 0)
  in
  let res = ref [] in
  let i = ref 0 in
  while !i + n <= m do
    if String.equal (String.sub text ~pos:!i ~len:n) prefix then (
      let start = !i + n in
      let j = ref start in
      while !j < m && is_ident text.[!j] do
        Int.incr j
      done;
      res := String.sub text ~pos:start ~len:(!j - start) :: !res;
      i := !j);
    Int.incr i
  done;
  List.rev !res

(** [sort_uniq] = [sort -u]. *)
let sort_uniq (xs : string list) : string list =
  List.dedup_and_sort xs ~compare:String.compare

(** [uniq] drops only *adjacent* duplicates (mirrors [uniq]). *)
let uniq (xs : string list) : string list =
  let rec loop acc = function
    | [] -> List.rev acc
    | [x] -> List.rev (x :: acc)
    | x :: tl ->
        (match acc with
        | h :: _ when String.equal h x -> loop acc tl
        | _ -> loop (x :: acc) tl)
  in
  loop [] xs

(** Drop the bare section names text/rodata/data/bss (mirrors the legacy
 *  [sed -E 's/\b(text|rodata|data|bss)\b//g'] applied to the word list). *)
let drop_std_sections (xs : string list) : string list =
  List.filter xs ~f:(fun s ->
    not
      (String.equal s "text" || String.equal s "rodata"
         || String.equal s "data" || String.equal s "bss"))

let regions_lds_h (config_lds_h : string) : string =
  let regions =
    grep_o "LDS_REGION_BASE_" config_lds_h |> sort_uniq
  in
  String.concat ~sep:"\n" (List.map regions ~f:(fun r -> "MEMORY_REGION(" ^ r ^ ")"))
  ^ (if List.is_empty regions then "" else "\n")

let sections_lds_h (config_lds_h : string) : string =
  let sections =
    grep_o "LDS_SECTION_VMA_" config_lds_h |> uniq |> drop_std_sections
  in
  String.concat
    ~sep:""
    (List.map sections ~f:(fun s ->
       Printf.sprintf
         ".%s : ALIGN(DATA_ALIGNMENT) {\n\t*(.%s)\n\t*(.%s.*)\n} SECTION_REGION(%s) :%s\n"
         s s s s s))

(** The 3rd whitespace-separated field of the first line containing
 *  [needle] (the legacy uses [grep <needle> | awk '{print $3}']). *)
let third_field (needle : string) (text : string) : string =
  match
    List.find_map (String.split text ~on:'\n') ~f:(fun l ->
      if Ctx.contains l needle then Some l else None)
  with
  | None -> ""
  | Some line ->
      (match String.split ~on:' ' line with
       | _a :: _b :: c :: _ -> c
       | _ -> "")

let phdrs_lds_h (config_lds_h : string) : string =
  let sections = grep_o "LDS_SECTION_PHDR_TYPE_" config_lds_h in
  String.concat
    ~sep:"\n"
    (List.map sections ~f:(fun s ->
       s ^ " "
       ^ third_field ("LDS_SECTION_PHDR_TYPE_" ^ s) config_lds_h
       ^ " "
       ^ third_field ("LDS_SECTION_PHDR_FLAGS_" ^ s) config_lds_h
       ^ ";"))
    ^ (if List.is_empty sections then "" else "\n")

(* ------------------------------------------------------------------ *)
(* section_symbols.lds.h                                               *)
(* ------------------------------------------------------------------ *)

(** The external-memory section a module type places [sec] into, if any. *)
let extmem_section (mt : Model.module_type) (sec : string) : string option =
  match Ctx.type_ann_param mt Ctx.ann_linker_section sec with
  | Some (Model.V_string s) -> Some s
  | _ -> None

(** True when the module has a @LinkerSection value for [sec] and that
 *  section is defined in config.lds.h (legacy [mod_in_extmem]). *)
let mod_in_extmem (mt : Model.module_type) (sec : string) (config_lds_h : string) :
    bool =
  match extmem_section mt sec with
  | None -> false
  | Some v -> Ctx.contains config_lds_h ("LDS_SECTION_VMA_" ^ v)

let section_symbols_lds_h (b : Model.build) (config_lds_h : string) : string =
  let secs = ["text"; "rodata"; "data"; "bss"] in
  let lines = ref [] in
  List.iter secs ~f:(fun s ->
    lines := ("/* " ^ s ^ " */") :: !lines;
    List.iter b.bd_modules ~f:(fun inst ->
      let mt = inst.mi_type in
      if mod_in_extmem mt s config_lds_h then (
        match extmem_section mt s with
        | Some v ->
            lines :=
              (Printf.sprintf "__SECTION_SYMBOLS(__module_%s_%s, .%s)"
                 (Model.fqn_to_id mt.mt_fqn) s v)
              :: !lines
        | None -> ()))
  );
  String.concat ~sep:"\n" (List.rev !lines) ^ "\n"

(* ------------------------------------------------------------------ *)
(* apps.lds.h                                                          *)
(* ------------------------------------------------------------------ *)

let lower (s : string) : string = Stdlib.String.lowercase_ascii s

(** Render apps.lds.h. [modules] is the build order; [apps] is the sorted
 *  set of @App modules plus their transitive build-deps (empty for
 *  scr5_32). [noapps] = modules minus apps. *)
let apps_lds_h (b : Model.build) (config_lds_h : string) : string =
  let modules = List.map b.bd_modules ~f:(fun inst -> inst.mi_type) in
  let is_app (mt : Model.module_type) : bool =
    List.exists (mt.mt_all_super @ [ mt ]) ~f:(fun t ->
      List.exists t.mt_annotations ~f:(fun a ->
        Model.fqn_equal a.ann_type.at_fqn Ctx.ann_app))
  in
  let fqn_cmp (a : Model.module_type) (b : Model.module_type) : int =
    String.compare (Model.fqn_to_string a.mt_fqn) (Model.fqn_to_string b.mt_fqn)
  in
  let apps = List.filter modules ~f:is_app |> List.dedup_and_sort ~compare:fqn_cmp in
  let app_set = Hashtbl.create (module String) in
  List.iter apps ~f:(fun (mt : Model.module_type) ->
    Hashtbl.set app_set ~key:(Model.fqn_to_string mt.mt_fqn) ~data:());
  let noapps =
    List.filter modules ~f:(fun (mt : Model.module_type) ->
      not (Hashtbl.mem app_set (Model.fqn_to_string mt.mt_fqn)))
  in
  let id (mt : Model.module_type) = Model.fqn_to_id mt.mt_fqn in
  let in_extmem (mt : Model.module_type) sec = mod_in_extmem mt sec config_lds_h in

  let lines = ref [] in
  let add s = lines := s :: !lines in

  add ("/* " ^ Ctx.banner ^ " */");
  add "";
  add "#include <asm-generic/embox.lds.h>";
  add "";
  add "#define __MODULE_ENRTY(section_, module_) \\";
  add "\t\t__module_##module_##_##section_##_vma = .; \\";
  add "\t\t*(.section_.module.module_); \\";
  add "\t\t*(.section_.*.module.module_); \\";
  add "\t\t__module_##module_##_##section_##_len = ABSOLUTE(. - \\";
  add "\t\t\tABSOLUTE(__module_##module_##_##section_##_vma));";
  add "";

  let print_section (mts : Model.module_type list) (sec : string) (name : string) =
    add ("#define LDS_" ^ name ^ " \\");
    add ("\t\t_" ^ lower name ^ "_start = .; \\");
    List.iter mts ~f:(fun (mt : Model.module_type) ->
      if not (in_extmem mt sec) then
        add ("\t\t__MODULE_ENRTY(" ^ sec ^ "," ^ id mt ^ ") \\"));
    add ("\t\t_" ^ lower name ^ "_end = .;");
    add ""
  in

  print_section apps "data" "APPS_DATA";
  print_section apps "bss" "APPS_BSS";

  add "/* Reserves memory to copy data. */";
  add "#define LDS_APP_DATA_RESERVE_BSS \\";
  add "\t\t_app_reserve_vma = .; \\";
  add "\t\t. += _apps_data_end - _apps_data_start + 4;";
  add "";

  print_section modules "text" "MODULES_TEXT";
  print_section modules "rodata" "MODULES_RODATA";
  print_section noapps "data" "MODULES_DATA";
  print_section noapps "bss" "MODULES_BSS";

  add "#define LDS_MODULES_DATA_LMA \\";
  List.iter modules ~f:(fun (mt : Model.module_type) ->
    if not (in_extmem mt "data") then (
      add ("\t\t__module_" ^ id mt ^ "_data_lma = _data_lma - _data_vma; \\");
      add ("\t\t__module_" ^ id mt ^ "_data_lma += __module_" ^ id mt
           ^ "_data_vma; \\")));
  add "";

  String.concat ~sep:"\n" (List.rev !lines) ^ "\n"

(* ------------------------------------------------------------------ *)
(* buildinfo.ld                                                        *)
(* ------------------------------------------------------------------ *)

(** Reproduce mk/gen_buildinfo.sh: [date | hexdump -v -e '"BYTE(0x" 1/1
 *  "%02X" ")\n"'] followed by [BYTE(0x00)]. The date is the current
 *  local time (LC_ALL=C), so the file differs between builds, exactly as
 *  in the legacy. *)
let buildinfo_ld () : string =
  let date_str = run_sh "LC_ALL=C date" in
  let bytes =
    (String.to_list date_str |> List.map ~f:(fun c -> Char.to_int c)) @ [0]
  in
  String.concat ~sep:"\n" (List.map bytes ~f:(fun i -> Printf.sprintf "BYTE(0x%02X)" i))
  ^ "\n"

(* ------------------------------------------------------------------ *)
(* image.lds.S                                                         *)
(* ------------------------------------------------------------------ *)

let image_lds_s (root : string) : string =
  read_file (root ^ "/mk/image.lds.S")

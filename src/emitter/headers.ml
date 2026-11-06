(* Generation of the per-module config and module headers.
 *
 * Byte-compatible with the legacy GNU Make emitter (mk/ugly.mk,
 * __module_config_h_template and __module_header_h_template).
 *
 * Whitespace semantics of the legacy templates, reproduced here:
 *   - [$(\n)] emits a newline, [$(\t)] a tab, [$(\h)] nothing;
 *   - blank lines in the template body are dropped;
 *   - [$(for x <- list, body)] is a space-joined foreach (empty
 *     iterations still leave their joining space);
 *   - adjacent template pieces are concatenated verbatim.
 *
 * Written in Base (not Core) per the Core -> Base migration plan.
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

(** [config_header s inst] renders the header for type [s] owned by
 *  instance [inst] ([s] is either [inst]'s type or one of its super
 *  types). *)
let config_header (s : Model.module_type) (inst : Model.module_instance) : string =
  let t = inst.mi_type in
  let sid = Model.fqn_to_id s.mt_fqn in
  let branch =
    if Model.fqn_equal s.mt_fqn t.mt_fqn then
      (* Most specific implementation type: define every bound option
       * under the most specific type's id, then alias every super type's
       * option ids to those. *)
      let own =
        if List.is_empty inst.mi_options then
          "// (no config options)\n"
        else
          String.concat ~sep:" "
            (List.map inst.mi_options ~f:(fun oi ->
               let id = Ctx.inherited_option_id oi.oi_option t in
               Printf.sprintf "\n#ifndef %s\n#define %s %s\n#endif\n" id id
                 (Ctx.render_value oi.oi_value)))
      in
      let supers =
        String.concat ~sep:" "
          (List.map t.mt_all_super ~f:(fun s2 ->
             if List.is_empty s2.mt_all_options then
               ""
             else
               String.concat ~sep:" "
                 (List.map s2.mt_all_options ~f:(fun o ->
                    let id = Ctx.option_id o in
                    let iid = Ctx.inherited_option_id o t in
                    Printf.sprintf "\n#ifndef %s\n#define %s \\\n\t\t%s\n#endif\n"
                      id id iid))))
      in
      "// This is the most specific implementation type\n" ^ own ^ supers
    else
      (* Base type: forward to the most specific type's header. *)
      "// This is a base type of " ^ Model.fqn_to_string t.mt_fqn ^ "\n"
      ^ "#include <config/" ^ Model.fqn_to_path t.mt_fqn ^ ".h>\n"
  in
  "/* " ^ Ctx.banner ^ " */\n"
  ^ "\n"
  ^ "#ifndef __CONFIG__" ^ sid ^ "__H_\n"
  ^ "#define __CONFIG__" ^ sid ^ "__H_\n"
  ^ branch
  ^ "\n"
  ^ "#endif /* __CONFIG__" ^ sid ^ "__H_ */\n"

(** Repo-relative path of a source file (Mybuild dir + declared name). *)
let source_rel_path (impl : Model.module_type) (src : Model.source) : string =
  let dir = Stdlib.Filename.dirname impl.mt_file in
  if String.equal dir "." then src.src_file_name
  else dir ^ "/" ^ src.src_file_name

(** [module_header s inst] renders the module header for type [s] owned by
 *  instance [inst]. *)
let module_header (s : Model.module_type) (inst : Model.module_instance) : string =
  let t = inst.mi_type in
  let sid = Model.fqn_to_id s.mt_fqn in
  let headers_of (impl : Model.module_type) : string list =
    List.filter_map impl.mt_sources ~f:(fun src ->
      if String.is_suffix ~suffix:".h" src.src_file_name
      then Some (source_rel_path impl src)
      else None)
  in
  let branch =
    if Model.fqn_equal s.mt_fqn t.mt_fqn then
      let impls =
        String.concat ~sep:" "
          (List.map ([ t ] @ t.mt_all_super) ~f:(fun impl ->
             let hs = headers_of impl in
             let inc =
               if List.is_empty hs then
                 "// (no headers to include)\n"
               else
                 String.concat ~sep:" "
                   (List.map hs ~f:(fun h -> "#include <../../" ^ h ^ ">\n"))
             in
             "\n// " ^ Model.fqn_to_string impl.mt_fqn ^ "\n" ^ inc))
      in
      "// This is the most specific implementation type\n"
      ^ "#include <config/" ^ Model.fqn_to_path t.mt_fqn ^ ".h>\n"
      ^ impls
    else
      "// This is a base type of " ^ Model.fqn_to_string t.mt_fqn ^ "\n"
      ^ "#include <module/" ^ Model.fqn_to_path t.mt_fqn ^ ".h>\n"
  in
  "/* " ^ Ctx.banner ^ " */\n"
  ^ "\n"
  ^ "#ifndef __MODULE__" ^ sid ^ "__H_\n"
  ^ "#define __MODULE__" ^ sid ^ "__H_\n"
  ^ "\n"
  ^ branch
  ^ "\n"
  ^ "#endif /* __MODULE__" ^ sid ^ "__H_ */\n"

(** Generate all config/module header pairs. Returns the number of pairs. *)
let gen_all (ctx : Ctx.t) : int =
  let targets = Ctx.header_targets ctx.build in
  List.iter targets ~f:(fun (s, inst) ->
    let path = Model.fqn_to_path s.mt_fqn in
    write_file (ctx.out_dir ^ "/include/config/" ^ path ^ ".h")
      (config_header s inst);
    write_file (ctx.out_dir ^ "/include/module/" ^ path ^ ".h")
      (module_header s inst));
  List.length targets

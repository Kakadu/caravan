(* Generation of gen/src/framework/mod/mod_inject.c.
 *
 * Byte-compatible with the legacy GNU Make emitter (mk/script/mod-inject.mk
 * + c-runtime-inject.mk). The file injects, for every module in the build,
 * its security label, logger, application/command registrations, runtime
 * dependencies and final module definition into the C runtime.
 *
 * Written in Base (not Core) per the Core -> Base migration plan.
 *)
open Base

let id_of (inst : Model.module_instance) = Model.mi_id inst

(** Render an annotation string parameter as an escaped C string literal.
 *  Missing parameters render as an empty literal (the legacy applies no
 *  annotation-option defaults here). *)
let cmd_str (inst : Model.module_instance) (fqn : Model.fqn) (param : string) : string =
  match Ctx.ann_param inst fqn param with
  | Some v -> Ctx.str_escape (Ctx.render_value v)
  | None -> "\"\""

let cmd_def (inst : Model.module_instance) (fqn : Model.fqn) : string =
  Printf.sprintf "MOD_CMD_DEF(%s, %s, %s, %s);"
    (id_of inst)
    (cmd_str inst fqn "name")
    (cmd_str inst fqn "help")
    (cmd_str inst fqn "man")

(** Render the whole mod_inject.c content. *)
let render (b : Model.build) : string =
  let modules = b.bd_modules in
  let lines = ref [] in
  let add s = lines := s :: !lines in
  let add_blank () = add "" in

  add ("/* " ^ Ctx.banner ^ " */");
  add_blank ();
  add "#include <framework/mod/embuild.h>";
  add_blank ();

  add "/* Security labels. */";
  List.iter modules ~f:(fun inst -> add (Printf.sprintf "MOD_LABEL_DEF(%s);" (id_of inst)));
  add_blank ();

  add "/* Loggers. */";
  List.iter modules ~f:(fun inst ->
    match Ctx.option_value_string inst "log_level" with
    | Some v -> add (Printf.sprintf "MOD_LOGGER_DEF(%s, %s);" (id_of inst) v)
    | None -> ());
  add_blank ();

  add "/* Applications. */";
  List.iter modules ~f:(fun inst ->
    if Ctx.has_ann_on_all_types inst Ctx.ann_app then
      add (Printf.sprintf "MOD_APP_DEF(%s);" (id_of inst)));
  add_blank ();

  add "/* Commands. */";
  List.iter modules ~f:(fun inst ->
    if Ctx.has_ann_on_all_types inst Ctx.ann_cmd then add (cmd_def inst Ctx.ann_cmd));
  add_blank ();

  add "/* C++ commands. */";
  List.iter modules ~f:(fun inst ->
    if Ctx.has_ann_on_all_types inst Ctx.ann_cxx_cmd then add (cmd_def inst Ctx.ann_cxx_cmd));
  add_blank ();

  add "/* Dependencies. */";
  List.iter modules ~f:(fun inst ->
    List.iter (Ctx.runtime_deps inst) ~f:(fun dep ->
      add (Printf.sprintf "MOD_DEP_DEF(%s, %s);" (id_of inst) (id_of dep))));
  add_blank ();

  add "/* Module build information. */";
  List.iter modules ~f:(fun inst -> add (Printf.sprintf "MOD_DEF(%s);" (id_of inst)));
  add_blank ();

  String.concat ~sep:"\n" (List.rev !lines) ^ "\n"

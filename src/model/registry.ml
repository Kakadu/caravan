(* Registry: file discovery, parsing, entity registration and linking.
 *
 * The registry loads all Mybuild files (in the same order as the legacy
 * build's 'find' invocation), parses them, registers all entities by
 * fully qualified name, and then links references in a batch.
 *)
open Base
open Model

(* Base.Hashtbl.create expects a module providing [compare], [hash] and
 * [sexp_of_t] (unlike Core, there is no Hashtbl.Key module type to
 * constrain it with). *)
module FqnHash = struct
  type t = fqn
  let compare = fqn_compare
  let sexp_of_t fqn = Sexp.List (List.map fqn ~f:(fun s -> Sexp.Atom s))
  let t_of_sexp = function
    | Sexp.List xs ->
        List.map xs ~f:(function
          | Sexp.Atom s -> s
          | _ -> invalid_arg "FqnHash.t_of_sexp")
    | _ -> invalid_arg "FqnHash.t_of_sexp"
  let hash = fqn_hash
end

type t = {
  root : string;
  files : string list;
  module_types : (fqn, module_type) Hashtbl.t;
  (** All module types in file (resource set) order. *)
  module_type_order : module_type list;
  features : (fqn, feature) Hashtbl.t;
  annotation_types : (fqn, annotation_type) Hashtbl.t;
  issues : issue list ref;
}

let create ~root ~files =
  {
    root;
    files;
    module_types = Hashtbl.create ~size:4096 (module FqnHash);
    module_type_order = [];
    features = Hashtbl.create ~size:64 (module FqnHash);
    annotation_types = Hashtbl.create ~size:256 (module FqnHash);
    issues = ref [];
  }

let add_issue t severity ?origin message =
  t.issues :=
    { iss_severity = severity; iss_message = message; iss_origin = origin }
    :: !(t.issues)

let issues t = List.rev !(t.issues)

let has_errors t = List.exists (issues t) ~f:(fun i -> Poly.equal i.iss_severity Error)

let report_issues t =
  List.iter (issues t) ~f:(fun i -> Stdlib.Format.eprintf "%s\n" (show_issue i))

(* ------------------------------------------------------------------ *)
(* File discovery                                                      *)
(* ------------------------------------------------------------------ *)

(** Discover Mybuild files exactly like mk/load-mybuild.inc.mk does:
 * {@code find mk/ src/ platform/ project/ third-party/ [- ext_project/]
 * -depth \( -name Mybuild -o -name '*.my' \) -print}. Shelling out to
 * 'find' guarantees the same traversal order as the legacy build. *)
let discover_files root =
  (* ext_project/ is always passed; find skips it (silently) when absent,
   * preserving the traversal order of the other directories. *)
  let dirs = ["mk/"; "src/"; "platform/"; "project/"; "third-party/"; "ext_project/"] in
  let cmd =
    "cd " ^ root ^ " && find " ^ String.concat ~sep:" " dirs
    ^ " -depth \\( -name Mybuild -o -name \\*.my \\) -print 2>/dev/null"
  in
  let (ic, oc) = Unix.open_process cmd in
  let files = In_channel.input_lines ic in
  let () = Stdlib.In_channel.close_noerr ic in
  let () = Stdlib.Out_channel.close_noerr oc in
  List.filter files ~f:(fun l -> not (String.is_empty l))

(* ------------------------------------------------------------------ *)
(* Parsing                                                             *)
(* ------------------------------------------------------------------ *)

let parse_myfile root file =
  let ic = In_channel.open_text (root ^ "/" ^ file) in
  let source = In_channel.input_all ic in
  In_channel.close ic;
  match Mybuild_parser.Parselib.parse_with_err ~source ~filename:file with
  | Some ast -> ast
  | None ->
    (* parse_with_err already reported the error. *)
    Stdlib.exit 1

(* ------------------------------------------------------------------ *)
(* Markers and conversion helpers                                      *)
(* ------------------------------------------------------------------ *)

module Ast = Mybuild_parser.Ast

let origin_of_file file = { file; line = 0 }

let marker_module fqn file =
  {
    mt_fqn = fqn;
    mt_name = List.last_exn fqn;
    mt_package = List.drop_last_exn fqn;
    mt_file = file;
    mt_origin = { file; line = 0 };
    mt_is_abstract = false;
    mt_is_static = false;
    mt_super_ref = None;
    mt_super = None;
    mt_all_super = [];
    mt_sub_types = [];
    mt_all_sub_types = [];
    mt_options = [];
    mt_all_options = [];
    mt_sources = [];
    mt_depends = [];
    mt_provides_refs = [];
    mt_provides = [];
    mt_requires_refs = [];
    mt_requires = [];
    mt_annotations = [];
  }

let marker_feature fqn file =
  {
    ft_fqn = fqn;
    ft_name = List.last_exn fqn;
    ft_file = file;
    ft_super_refs = [];
    ft_supers = [];
    ft_sub_features = [];
    ft_all_sub_features = [];
    ft_annotations = [];
  }

let marker_annotation fqn file =
  {
    at_fqn = fqn;
    at_name = List.last_exn fqn;
    at_file = file;
    at_options = [];
    at_origin = { file; line = 0 };
  }

let dummy_annotation : annotation_type =
  {
    at_fqn = [];
    at_name = "";
    at_file = "";
    at_options = [];
    at_origin = { file = ""; line = 0 };
  }

let dummy_module : module_type = marker_module ["__dummy__"] ""

let convert_value = function
  | Ast.StringLiteral s -> V_string s
  | Ast.NumberLiteral { lexeme; value } -> V_number { lexeme; value }
  | Ast.BooleanLiteral b -> V_bool b
  | Ast.Refer r -> V_ref r

let convert_annotation ctx ~package (type_ref, arg) origin =
  {
    ann_type_ref = type_ref;
    ann_type = dummy_annotation;
    ann_arg =
      (match arg with
       | None -> None
       | Some (Ast.Params ps) ->
           Some (Ann_params (List.map ps ~f:(fun (n, v) -> (n, convert_value v))))
       | Some (Ast.Value v) -> Some (Ann_value (convert_value v)));
    ann_origin = origin;
    ann_ctx = ctx;
    ann_package = package;
  }

let convert_annotations ctx ~package anns origin =
  List.map anns ~f:(fun a -> convert_annotation ctx ~package a origin)

let convert_option def anns origin : option_def =
  let (def_type, name, default) = def in
  let anns = (anns : annotation list) in
  let origin = (origin : origin) in
  let opt_kind, opt_ref_ref =
    match def_type with
    | Ast.StringType -> String, None
    | Ast.NumberType -> Number, None
    | Ast.BooleanType -> Boolean, None
    | Ast.ReferType r -> TypeRef, Some r
  in
  {
    opt_name = name;
    opt_kind;
    opt_ref_ref;
    opt_ref = None;
    opt_default =
      (match default with
       | None -> None
       | Some v -> Some (convert_value v));
    (* [anns] arrives already converted to model annotations by the caller. *)
    opt_annotations = anns;
    opt_container = C_module dummy_module;
    opt_origin = origin;
  }

let convert_source file_name is_object anns module_ origin =
  let anns = (anns : annotation list) in
  let origin = (origin : origin) in
  {
    src_file_name = file_name;
    src_is_object = is_object;
    (* [anns] arrives already converted to model annotations by the caller. *)
    src_annotations = anns;
    src_module = module_;
    src_origin = origin;
  }

let convert_module file package anns (modifiers, name, super_ref, members) =
  let fqn = fqn_append package name in
  let origin = origin_of_file file in
  let is_abstract = List.mem modifiers Ast.Abstract ~equal:Poly.equal in
  let is_static = List.mem modifiers Ast.Static ~equal:Poly.equal in
  let mt =
    {
      mt_fqn = fqn;
      mt_name = name;
      mt_package = package;
      mt_file = file;
      mt_origin = origin;
      mt_is_abstract = is_abstract;
      mt_is_static = is_static;
      mt_super_ref = super_ref;
      mt_super = None;
      mt_all_super = [];
      mt_sub_types = [];
      mt_all_sub_types = [];
      mt_options = [];
      mt_all_options = [];
      mt_sources = [];
      mt_depends = [];
      mt_provides_refs = [];
      mt_provides = [];
      mt_requires_refs = [];
      mt_requires = [];
      (* Entity-level annotations (e.g. @DefaultImpl before the module). *)
      mt_annotations = convert_annotations (Some fqn) ~package anns origin;
    }
  in
  let (options, sources, depends, provides_refs, requires_refs) =
    List.fold members ~init:([], [], [], [], []) ~f:(fun acc (anns, member) ->
      let anns = convert_annotations (Some fqn) ~package anns origin in
      match member with
      | Ast.Opt def ->
          let (o, s, d, p, r) = acc in
          (convert_option def anns origin :: o, s, d, p, r)
      | Ast.Source names ->
          let (o, s, d, p, r) = acc in
          (o,
           s
           @ List.map names ~f:(fun n ->
                 convert_source n false anns mt origin),
           d,
           p,
           r)
      | Ast.Object names ->
          let (o, s, d, p, r) = acc in
          (o,
           s
           @ List.map names ~f:(fun n ->
                 convert_source n true anns mt origin),
           d,
           p,
           r)
      | Ast.Depends refs ->
          let (o, s, d, p, r) = acc in
          (o,
           s,
           d
           @ [
             {
               dep_module_refs = refs;
               dep_modules = [];
               dep_annotations = anns;
               dep_origin = origin;
             }
           ],
           p,
           r)
      | Ast.Provides refs ->
          let (o, s, d, p, r) = acc in
          (o, s, d, p @ refs, r)
      | Ast.Requires refs ->
          let (o, s, d, p, r) = acc in
          (o, s, d, p, r @ refs))
  in
  {
    mt with
    mt_options = List.rev options;
    mt_sources = sources;
    mt_depends = depends;
    mt_provides_refs = provides_refs;
    mt_requires_refs = requires_refs;
  }

let convert_annotation_type file package (name, members) =
  let fqn = fqn_append package name in
  let origin = origin_of_file file in
  let options =
    List.map members ~f:(fun (anns, def) ->
      convert_option def
        (convert_annotations (Some fqn) ~package anns origin)
        origin)
  in
  {
    at_fqn = fqn;
    at_name = name;
    at_file = file;
    at_options = options;
    at_origin = origin;
  }

(* ------------------------------------------------------------------ *)
(* Registration (pass 1): collect entity names in file order.          *)
(* ------------------------------------------------------------------ *)

let register_entity t kind fqn file =
  (* Reserve the slot with a marker; real objects are created in pass 2. *)
  match kind with
  | `Interface ->
      add_issue t Error ~origin:{ file; line = 0 }
        (Printf.sprintf "Interfaces are not supported (entity '%s')" (fqn_to_string fqn))
  | `Module ->
      if Hashtbl.mem t.module_types fqn then
        add_issue t Error ~origin:{ file; line = 0 }
          (Printf.sprintf "Duplicate entity '%s'" (fqn_to_string fqn))
      else Hashtbl.set t.module_types ~key:fqn ~data:(marker_module fqn file)
  | `Feature ->
      if Hashtbl.mem t.features fqn then
        add_issue t Error ~origin:{ file; line = 0 }
          (Printf.sprintf "Duplicate entity '%s'" (fqn_to_string fqn))
      else Hashtbl.set t.features ~key:fqn ~data:(marker_feature fqn file)
  | `Annotation ->
      if Hashtbl.mem t.annotation_types fqn then
        add_issue t Error ~origin:{ file; line = 0 }
          (Printf.sprintf "Duplicate entity '%s'" (fqn_to_string fqn))
      else Hashtbl.set t.annotation_types ~key:fqn ~data:(marker_annotation fqn file)


(* ------------------------------------------------------------------ *)
(* Resolution                                                          *)
(* ------------------------------------------------------------------ *)

(** Candidate order for a reference, mirroring the legacy linker:
 * container fqn, file package, mybuild.lang, then the full name. *)
let candidates ctx_fqn ~package ref =
  let ctx =
    match ctx_fqn with
    | Some f when not (fqn_is_empty f) -> [f @ ref]
    | _ -> []
  in
  let pkg = if fqn_is_empty package then [ref] else [package @ ref] in
  ctx @ pkg @ [["mybuild"; "lang"] @ ref; ref]

let resolve_in t table kind_name ctx_fqn ~package refname origin =
  let seen = ref [] in
  let hits =
    List.filter_map (candidates ctx_fqn ~package refname) ~f:(fun c ->
      if List.mem !seen c ~equal:fqn_equal then None
      else (
        seen := c :: !seen;
        Hashtbl.find table c))
  in
  match hits with
  | [] ->
      add_issue t Error ~origin
        (Printf.sprintf "Unresolved %s reference '%s'" kind_name (fqn_to_string refname));
      None
  | [hit] -> Some hit
  | hits ->
      add_issue t Warning ~origin
        (Printf.sprintf "Multiple matches for %s reference '%s'; using the first"
           kind_name (fqn_to_string refname));
      Some (List.hd_exn hits)

let resolve_module t ctx_fqn ~package ref origin =
  resolve_in t t.module_types "module" ctx_fqn ~package ref origin

let resolve_feature t ctx_fqn ~package ref origin =
  resolve_in t t.features "feature" ctx_fqn ~package ref origin

let resolve_annotation_type t ctx_fqn ~package ref origin =
  resolve_in t t.annotation_types "annotation type" ctx_fqn ~package ref origin

(** Resolve a value to a module type (for type-reference values). *)
let value_as_module t ctx_fqn ~package value =
  match value with
  | V_module mt -> Some mt
  | V_ref r ->
      (match resolve_module t ctx_fqn ~package r { file = ""; line = 0 } with
       | Some mt -> Some mt
       | None -> None)
  | _ -> None

(* ------------------------------------------------------------------ *)
(* Linking (batch resolution of all references)                        *)
(* ------------------------------------------------------------------ *)

let all_module_types t = t.module_type_order

let all_features t = Hashtbl.to_alist t.features |> List.map ~f:snd

let all_annotation_types t = Hashtbl.to_alist t.annotation_types |> List.map ~f:snd

let all_annotations t =
  List.concat_map (all_module_types t) ~f:(fun mt ->
    mt.mt_annotations
    @ List.concat_map mt.mt_options ~f:(fun o -> o.opt_annotations)
    @ List.concat_map mt.mt_sources ~f:(fun s -> s.src_annotations)
    @ List.concat_map mt.mt_depends ~f:(fun d -> d.dep_annotations))
  @ List.concat_map (all_annotation_types t) ~f:(fun at ->
        List.concat_map at.at_options ~f:(fun o -> o.opt_annotations))
  @ List.concat_map (all_features t) ~f:(fun ft -> ft.ft_annotations)

let all_options t =
  List.concat_map (all_module_types t) ~f:(fun mt -> mt.mt_options)
  @ List.concat_map (all_annotation_types t) ~f:(fun at -> at.at_options)

let rec all_super_chain mt =
  match mt.mt_super with
  | None -> []
  | Some s -> s :: all_super_chain s

let link t =
  (* 1. Module hierarchy. *)
  List.iter (all_module_types t) ~f:(fun mt ->
    mt.mt_super <-
      (match mt.mt_super_ref with
       | None -> None
       | Some ref ->
           resolve_module t (Some mt.mt_fqn) ~package:mt.mt_package ref mt.mt_origin));
  List.iter (all_module_types t) ~f:(fun mt -> mt.mt_all_super <- all_super_chain mt);
  List.iter (all_module_types t) ~f:(fun mt ->
    match mt.mt_super with
    | None -> ()
    | Some s -> s.mt_sub_types <- s.mt_sub_types @ [mt]);
  (* Transitive sub types (DFS preorder, as in the legacy getter). *)
  let memo = Hashtbl.create ~size:4096 (module FqnHash) in
  let rec dfs mt =
    match Hashtbl.find memo mt.mt_fqn with
    | Some l -> l
    | None ->
        let l = List.concat_map mt.mt_sub_types ~f:(fun s -> s :: dfs s) in
        Hashtbl.set memo ~key:mt.mt_fqn ~data:l;
        l
  in
  List.iter (all_module_types t) ~f:(fun mt -> mt.mt_all_sub_types <- dfs mt);
  (* 2. All options (with name shadowing). *)
  let memo_opts = Hashtbl.create ~size:4096 (module FqnHash) in
  let rec dfs_opts mt =
    match Hashtbl.find memo_opts mt.mt_fqn with
    | Some l -> l
    | None ->
        let own_names = List.map mt.mt_options ~f:(fun o -> o.opt_name) in
        let inherited =
          match mt.mt_super with
          | None -> []
          | Some s ->
              List.filter (dfs_opts s) ~f:(fun so ->
                not (List.mem own_names so.opt_name ~equal:String.equal))
        in
        let l = mt.mt_options @ inherited in
        Hashtbl.set memo_opts ~key:mt.mt_fqn ~data:l;
        l
  in
  List.iter (all_module_types t) ~f:(fun mt -> mt.mt_all_options <- dfs_opts mt);
  (* 3. Depends / provides / requires. *)
  List.iter (all_module_types t) ~f:(fun mt ->
    List.iter mt.mt_depends ~f:(fun dm ->
      dm.dep_modules <-
        List.filter_map dm.dep_module_refs ~f:(fun ref ->
          resolve_module t (Some mt.mt_fqn) ~package:mt.mt_package ref dm.dep_origin));
    mt.mt_provides <-
      List.filter_map mt.mt_provides_refs ~f:(fun ref ->
        resolve_feature t (Some mt.mt_fqn) ~package:mt.mt_package ref mt.mt_origin);
    mt.mt_requires <-
      List.filter_map mt.mt_requires_refs ~f:(fun ref ->
        resolve_feature t (Some mt.mt_fqn) ~package:mt.mt_package ref mt.mt_origin));
  (* 4. Feature hierarchy. *)
  List.iter (all_features t) ~f:(fun ft ->
    ft.ft_supers <-
      List.filter_map ft.ft_super_refs ~f:(fun ref ->
        resolve_feature t (Some ft.ft_fqn) ~package:(List.drop_last_exn ft.ft_fqn) ref
          { file = ft.ft_file; line = 0 }));
  List.iter (all_features t) ~f:(fun ft ->
    List.iter ft.ft_supers ~f:(fun s -> s.ft_sub_features <- s.ft_sub_features @ [ft]));
  let memo_ft = Hashtbl.create ~size:64 (module FqnHash) in
  let rec dfs_ft ft =
    match Hashtbl.find memo_ft ft.ft_fqn with
    | Some l -> l
    | None ->
        let l = List.concat_map ft.ft_sub_features ~f:(fun s -> s :: dfs_ft s) in
        Hashtbl.set memo_ft ~key:ft.ft_fqn ~data:l;
        l
  in
  List.iter (all_features t) ~f:(fun ft -> ft.ft_all_sub_features <- dfs_ft ft);
  (* 5. Option containers.
   *
   * Note: option type references (e.g. [Module value] in an annotation) are
   * deliberately NOT resolved. The legacy linker leaves them empty (see the
   * commented-out link code in mk/mybuild/myfile-linker.mk); all six such
   * options in the tree (mk/mybuild/lang/*.my) stay unresolved. The raw
   * reference is kept in [opt_ref_ref] for reference. *)
  List.iter (all_module_types t) ~f:(fun mt ->
    List.iter mt.mt_options ~f:(fun o -> o.opt_container <- C_module mt));
  List.iter (all_annotation_types t) ~f:(fun at ->
    List.iter at.at_options ~f:(fun o -> o.opt_container <- C_annotation at));
  (* 6. Annotations. *)
  List.iter (all_annotations t) ~f:(fun a ->
    a.ann_type <-
      (match
         resolve_annotation_type t a.ann_ctx ~package:a.ann_package a.ann_type_ref
           a.ann_origin
       with
       | Some at -> at
       | None -> dummy_annotation))

(* ------------------------------------------------------------------ *)
(* Loading                                                             *)
(* ------------------------------------------------------------------ *)

(** Load and link all Mybuild files. *)
let load ~root =
  let files = discover_files root in
  let t = create ~root ~files in
  let parsed = List.map files ~f:(fun file -> (file, parse_myfile root file)) in
  (* Pass 1: register entity names (duplicate detection, existence). *)
  List.iter parsed ~f:(fun (file, ast) ->
    match ast with
    | (package, _imports, entities) ->
        let package = Option.value ~default:[] package in
        List.iter entities ~f:(fun (_anns, entity) ->
          match entity with
          | Ast.Module (_mods, name, _super, _members) ->
              register_entity t `Module (fqn_append package name) file
          | Ast.Annotation (name, _members) ->
              register_entity t `Annotation (fqn_append package name) file
          | Ast.Interface (name, _supers, _features) ->
              register_entity t `Interface (fqn_append package name) file));
  (* Pass 2: create real objects, replacing markers. *)
  let module_type_order =
    List.concat_map parsed ~f:(fun (file, ast) ->
      match ast with
      | (package, _imports, entities) ->
          let package = Option.value ~default:[] package in
          List.filter_map entities ~f:(fun (anns, entity) ->
            match entity with
            | Ast.Module (mods, name, super, members) ->
                let mt = convert_module file package anns (mods, name, super, members) in
                Hashtbl.set t.module_types ~key:mt.mt_fqn ~data:mt;
                Some mt
            | Ast.Annotation (name, members) ->
                let at = convert_annotation_type file package (name, members) in
                Hashtbl.set t.annotation_types ~key:at.at_fqn ~data:at;
                None
            | Ast.Interface _ -> None))
  in
  let t = { t with module_type_order } in
  link t;
  t

(* ------------------------------------------------------------------ *)
(* Configuration files                                                 *)
(* ------------------------------------------------------------------ *)

module CfgAst = Configfile_parser.Ast

let cfg_convert_value = function
  | CfgAst.Str s -> V_string s
  | CfgAst.Num { lexeme; value } -> V_number { lexeme; value }
  | CfgAst.Bool b -> V_bool b
  | CfgAst.Ref r -> V_ref r

let cfg_convert_annotation ctx ~package (type_ref, arg) origin =
  {
    ann_type_ref = type_ref;
    ann_type = dummy_annotation;
    ann_arg =
      (match arg with
       | None -> None
       | Some (CfgAst.Params ps) ->
           Some (Ann_params (List.map ps ~f:(fun (n, v) -> (n, cfg_convert_value v))))
       | Some (CfgAst.Value v) -> Some (Ann_value (cfg_convert_value v)));
    ann_origin = origin;
    ann_ctx = ctx;
    ann_package = package;
  }

(** Load and link a configuration file (conf/mods.conf). The registry
 * must already be loaded. *)
let load_config t file =
  let ic = In_channel.open_text (t.root ^ "/" ^ file) in
  let source = In_channel.input_all ic in
  In_channel.close ic;
  let cf =
    match Configfile_parser.Parselib.parse_with_err ~source ~filename:file with
    | Some cf -> cf
    | None -> Stdlib.exit 1
  in
  let package = Option.value ~default:[] cf.CfgAst.package in
  let cfg =
    match cf.CfgAst.configurations with
    | [cfg] -> cfg
    | [] ->
        add_issue t Error ~origin:{ file; line = 0 }
          (Printf.sprintf "No configuration in '%s'" file);
        { CfgAst.name = ""; CfgAst.includes = []; CfgAst.annotations = [] }
    | _ ->
        add_issue t Error ~origin:{ file; line = 0 }
          (Printf.sprintf "Multiple configurations in '%s'" file);
        List.hd_exn cf.CfgAst.configurations
  in
  let includes =
    List.map cfg.CfgAst.includes ~f:(fun inc ->
      let origin = { file; line = 0 } in
      let mt =
        match resolve_module t None ~package inc.CfgAst.module_ref origin with
        | Some mt -> mt
        | None -> dummy_module
      in
      {
        ci_module = mt;
        ci_bindings =
          List.map inc.CfgAst.bindings ~f:(fun (n, v) -> (n, cfg_convert_value v));
        ci_annotations =
          List.map inc.CfgAst.annotations ~f:(fun a ->
            cfg_convert_annotation None ~package a origin);
        ci_origin = origin;
      })
  in
  (* Link annotations on the includes. *)
  List.iter includes ~f:(fun inc ->
    List.iter inc.ci_annotations ~f:(fun a ->
      a.ann_type <-
        (match resolve_annotation_type t None ~package a.ann_type_ref a.ann_origin with
         | Some at -> at
         | None -> dummy_annotation)));
  {
    cfg_name = cfg.CfgAst.name;
    cfg_package = package;
    cfg_file = file;
    cfg_includes = includes;
  }

(* Canonical Embox Mybuild model.
 *
 * This module defines the data model produced by parsing and linking
 * Mybuild files and configuration files. It mirrors the object model of
 * the legacy GNU Make build system (mk/mybuild/*.mk), restricted to the
 * parts needed to generate a build.
 *
 * References are stored twice: as a raw qualified name (the [*_ref]
 * fields) and as a resolved object (the mutable fields), which the
 * linker fills in a batch after all entities are created. This keeps
 * linking order-independent and cycle-safe.
 *)
open Base

(** Fully qualified name: a list of identifiers, e.g. ["embox"; "mem"; "heap_bm"]. *)
type fqn = string list

let fqn_of_string s = String.split ~on:'.' s
let fqn_to_string = String.concat ~sep:"."
let fqn_to_path = String.concat ~sep:"/"
let fqn_to_id = String.concat ~sep:"__"
let fqn_is_empty = List.is_empty
let fqn_append fqn name = fqn @ [ name ]
let fqn_equal = List.equal String.equal
let fqn_compare a b =
  let rec loop a b =
    match a, b with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | ha :: ta, hb :: tb ->
        let c = String.compare ha hb in
        if c = 0 then loop ta tb else c
  in
  loop a b
let fqn_hash fqn = Hashtbl.hash (fqn_to_string fqn)

(** Source location for diagnostics. *)
type origin = {
  file : string; (** Path relative to the repository root. *)
  line : int;
}

let show_origin origin = Printf.sprintf "%s:%d" origin.file origin.line

(* The core model: mutually recursive types.
 *
 * [value] and [annotation_arg] are part of the block because values can
 * refer to module types. *)
type value =
  | V_string of string
  | V_number of { lexeme : string; value : float }
  | V_bool of bool
  | V_ref of fqn
  | V_module of module_type

and annotation_arg =
  | Ann_params of (string * value) list
  | Ann_value of value

and container =
  | C_module of module_type
  | C_annotation of annotation_type

and annotation_type = {
  at_fqn : fqn;
  at_name : string;
  at_file : string;
  at_options : option_def list;
  at_origin : origin;
}

and annotation = {
  ann_type_ref : fqn;
  mutable ann_type : annotation_type;
  ann_arg : annotation_arg option;
  ann_origin : origin;
  (** FQN of the containing module/annotation type, if any. *)
  ann_ctx : fqn option;
  (** Package of the file where the annotation appears. *)
  ann_package : fqn;
}

and option_kind =
  | String
  | Number
  | Boolean
  | TypeRef

and option_def = {
  opt_name : string;
  opt_kind : option_kind;
  opt_ref_ref : fqn option; (** Referenced type for [TypeRef] options. *)
  mutable opt_ref : module_type option;
  opt_default : value option;
  opt_annotations : annotation list;
  mutable opt_container : container;
  opt_origin : origin;
}

and module_type = {
  mt_fqn : fqn;
  mt_name : string;
  mt_package : fqn;
  mt_file : string;
  mt_origin : origin;
  mt_is_abstract : bool;
  mt_is_static : bool;
  mt_super_ref : fqn option;
  mutable mt_super : module_type option;
  (** All proper super types, nearest first. *)
  mutable mt_all_super : module_type list;
  (** Direct sub types. *)
  mutable mt_sub_types : module_type list;
  (** All proper sub types. *)
  mutable mt_all_sub_types : module_type list;
  mt_options : option_def list;
  (** Own options plus inherited ones, with name shadowing. *)
  mutable mt_all_options : option_def list;
  mt_sources : source list;
  mt_depends : depends_member list;
  mt_provides_refs : fqn list;
  mutable mt_provides : feature list;
  mt_requires_refs : fqn list;
  mutable mt_requires : feature list;
  mt_annotations : annotation list;
}

and source = {
  src_file_name : string;
  src_is_object : bool;
  src_annotations : annotation list;
  src_module : module_type;
  src_origin : origin;
}

and depends_member = {
  dep_module_refs : fqn list;
  mutable dep_modules : module_type list;
  dep_annotations : annotation list;
  dep_origin : origin;
}

and feature = {
  ft_fqn : fqn;
  ft_name : string;
  ft_file : string;
  ft_super_refs : fqn list;
  mutable ft_supers : feature list;
  mutable ft_sub_features : feature list;
  mutable ft_all_sub_features : feature list;
  ft_annotations : annotation list;
}

let show_value = function
  | V_string s -> s
  | V_number { lexeme; _ } -> lexeme
  | V_bool b -> Bool.to_string b
  | V_ref r -> fqn_to_string r
  | V_module mt -> fqn_to_string mt.mt_fqn

let ann_param ann_arg name =
  match ann_arg with
  | Ann_params params ->
      if List.exists params ~f:(fun (n, _) -> String.equal n name)
      then Some (snd (List.find_exn params ~f:(fun (n, _) -> String.equal n name)))
      else None
  | Ann_value _ -> None

let ann_value ann_arg =
  match ann_arg with
  | Ann_value v -> Some v
  | Ann_params _ -> None

(** Strict subtyping: [is_subtype_of a b] iff [a] is a proper sub type of [b]
 * (i.e. [b] is in [a]'s super type chain, excluding [a] itself). Mirrors the
 * legacy [MyModuleType.isSubTypeOf] = [filter $1 allSuperTypes]. *)
let is_subtype_of a b =
  List.mem a.mt_all_super b ~equal:(fun x y -> fqn_equal x.mt_fqn y.mt_fqn)
let is_super_type_of a b = is_subtype_of b a

let is_abstract mt = mt.mt_is_abstract

(** A resolved configuration file include member. *)
type cfg_include = {
  ci_module : module_type;
  ci_bindings : (string * value) list;
  ci_annotations : annotation list;
  ci_origin : origin;
}

(** A configuration file (mods.conf). *)
type configuration = {
  cfg_name : string;
  cfg_package : fqn;
  cfg_file : string;
  cfg_includes : cfg_include list;
}

(** A bound option of a module instance. *)
type option_instance = {
  oi_option : option_def;
  oi_value : value;
}

(** A module instance in the build model. *)
type module_instance = {
  mutable mi_type : module_type;
  mutable mi_include_member : cfg_include option;
  mutable mi_depends : module_instance list;
  mutable mi_no_runtime_depends : module_instance list;
  mutable mi_options : option_instance list;
}

let mi_fqn inst = inst.mi_type.mt_fqn
let mi_path inst = fqn_to_path inst.mi_type.mt_fqn
let mi_id inst = fqn_to_id inst.mi_type.mt_fqn

type issue_severity =
  | Warning
  | Error

type issue = {
  iss_severity : issue_severity;
  iss_message : string;
  iss_origin : origin option;
}

let show_issue issue =
  let loc =
    match issue.iss_origin with
    | Some o -> " " ^ show_origin o
    | None -> ""
  in
  let sev =
    match issue.iss_severity with
    | Warning -> "warning"
    | Error -> "error"
  in
  Printf.sprintf "%s:%s %s" sev loc issue.iss_message

(** The inferred build model. *)
type build = {
  bd_configuration : configuration;
  (** Module instances in store (first-registration) order. *)
  bd_modules : module_instance list;
  bd_issues : issue list;
}

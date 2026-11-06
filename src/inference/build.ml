(* Build inference: derive the set of module instances (and their bound
 * options) from the linked Mybuild model and a configuration file.
 *
 * This is a faithful port of the legacy GNU Make inference in
 * mk/mybuild/mybuild.mk (class-Mybuild.createBuild and friends) plus the
 * Build-phase annotation callbacks in mk/mybuild/annotation_handlers/.
 *
 * The algorithm:
 *   1. Include every @Mandatory module type (file order).
 *   2. Include every configuration include (file order), recording the
 *      include member on the resulting instance.
 *   3. Run the builders over a snapshot of the current instances:
 *        - closeInstances: pull in each instance's dependencies (transitively).
 *        - specifyInstances: upgrade abstract instances to their recommended
 *          concrete implementation.
 *      When a builder adds a new instance, all builders active so far are
 *      re-applied to it (the legacy 'check' mechanism), giving a transitive
 *      fixpoint.
 *   4. Run the checkers (abstract realization, feature realization, option
 *      uniqueness, numeric constraints).
 *   5. Bind options for every instance.
 *)
open Base

module Reg = Registry

(* Annotation type FQNs that carry a Build-phase callback. *)
let f_mandatory = [ "mybuild"; "lang"; "Mandatory" ]
let f_no_runtime = [ "mybuild"; "lang"; "NoRuntime" ]
let f_default_impl = [ "mybuild"; "lang"; "DefaultImpl" ]
let f_for = [ "mybuild"; "lang"; "For" ]
let f_if_need = [ "mybuild"; "lang"; "IfNeed" ]
let f_test_for = [ "mybuild"; "lang"; "TestFor" ]
let f_with_test = [ "mybuild"; "lang"; "WithTest" ]
let f_with_all_tests = [ "mybuild"; "lang"; "WithAllTests" ]

let fqn_eq = Model.fqn_equal

(** What annotation callbacks may be attached to. *)
type target =
  | T_module of Model.module_type
  | T_include of Model.cfg_include

let annotations_of = function
  | T_module mt -> mt.mt_annotations
  | T_include ci -> ci.ci_annotations

type t = {
  reg : Reg.t;
  (** instance store, keyed by module FQN (an instance is registered under its
      type and all of its super types). *)
  store : (Model.fqn, Model.module_instance) Hashtbl.t;
  (** First-registration order of store keys; defines build.modules order. *)
  store_order : Model.fqn list ref;
  by_name : (Model.fqn, Model.module_instance) Hashtbl.t;
  active_features : (Model.fqn, Model.module_instance list) Hashtbl.t;
  recommendations : (Model.fqn, Model.module_type) Hashtbl.t;
  issues : Model.issue list ref;
  (** Builders activated so far, in activation order. *)
  current_builders : (t -> Model.module_instance list -> unit) list ref;
}

let create ~reg =
  {
    reg;
    store = Hashtbl.create ~size:1024 (module Reg.FqnHash);
    store_order = ref [];
    by_name = Hashtbl.create ~size:1024 (module Reg.FqnHash);
    active_features = Hashtbl.create ~size:256 (module Reg.FqnHash);
    recommendations = Hashtbl.create ~size:256 (module Reg.FqnHash);
    issues = ref [];
    current_builders = ref [];
  }

let add_issue b severity origin message =
  b.issues := { Model.iss_severity = severity; iss_message = message; iss_origin = origin }
  :: !(b.issues)

let issues b = List.rev !(b.issues)

let has_errors b = List.exists (issues b) ~f:(fun i -> Poly.equal i.iss_severity Model.Error)

(* Pointer-unique list, preserving first-seen order. *)
let unique_by_ptr lst =
  let seen = ref [] in
  List.filter lst ~f:(fun x ->
    if List.mem !(seen) x ~equal:(fun a c -> phys_equal a c) then false
    else (
      seen := x :: !(seen);
      true))

(* ------------------------------------------------------------------ *)
(* Annotation processing (Build-phase callbacks)                       *)
(* ------------------------------------------------------------------ *)

let value_as_module b ctx_fqn ~package value =
  Reg.value_as_module b.reg ctx_fqn ~package value

let handle_default_impl b (mt : Model.module_type) (ann : Model.annotation) =
  match ann.ann_arg with
  | None -> add_issue b Model.Error (Some ann.ann_origin) "@DefaultImpl expects a value"
  | Some arg ->
      (match arg with
       | Ann_value v ->
           (match value_as_module b (Some mt.mt_fqn) ~package:mt.mt_package v with
            | Some impl ->
                (match Hashtbl.find b.recommendations mt.mt_fqn with
                 | None -> Hashtbl.set b.recommendations ~key:mt.mt_fqn ~data:impl
                 | Some _ -> ())
            | None ->
                add_issue b Model.Error (Some ann.ann_origin)
                  (Printf.sprintf "@DefaultImpl value '%s' does not resolve to a module"
                     (Model.show_value v)))
       | Ann_params _ ->
           add_issue b Model.Error (Some ann.ann_origin) "@DefaultImpl expects a value")

(** Validate that [value] is a super type of the included module. Returns true
 * on success. *)
let check_for_parent b (ci : Model.cfg_include) (ann : Model.annotation) =
  match ann.ann_arg with
  | None ->
      add_issue b Model.Error (Some ann.ann_origin)
        (Printf.sprintf "Annotation @%s expects a value" ann.ann_type.at_name);
      false
  | Some arg ->
      (match arg with
       | Ann_value v ->
           (match value_as_module b (Some ci.ci_module.mt_fqn) ~package:ci.ci_module.mt_package v with
            | Some parent ->
                if Model.is_super_type_of parent ci.ci_module
                then true
                else (
                  add_issue b Model.Error (Some ann.ann_origin)
                    (Printf.sprintf "Annotation @%s value '%s' should be a parent of '%s'"
                       ann.ann_type.at_name
                       (Model.show_value v)
                       (Model.fqn_to_string ci.ci_module.mt_fqn));
                  false)
            | None ->
                add_issue b Model.Error (Some ann.ann_origin)
                  (Printf.sprintf "Annotation @%s value '%s' does not resolve to a module"
                     ann.ann_type.at_name (Model.show_value v));
                false)
       | Ann_params _ ->
           add_issue b Model.Error (Some ann.ann_origin)
             (Printf.sprintf "Annotation @%s expects a value" ann.ann_type.at_name);
           false)

let handle_for b (ci : Model.cfg_include) (ann : Model.annotation) = check_for_parent b ci ann

let handle_if_need b (ci : Model.cfg_include) (ann : Model.annotation) =
  if check_for_parent b ci ann then
    (match ann.ann_arg with
     | Some (Ann_value v) ->
         (match value_as_module b (Some ci.ci_module.mt_fqn) ~package:ci.ci_module.mt_package v with
          | Some value -> Hashtbl.set b.recommendations ~key:value.mt_fqn ~data:ci.ci_module
          | None -> ())
     | _ -> ())

let annotation_process b target =
  List.iter (annotations_of target) ~f:(fun ann ->
    let fqn = ann.ann_type.at_fqn in
    if fqn_eq fqn f_default_impl then
      (match target with
       | T_module mt -> handle_default_impl b mt ann
       | T_include _ ->
           add_issue b Model.Error (Some ann.ann_origin) "@DefaultImpl on a config include")
    else if fqn_eq fqn f_for then
      (match target with
       | T_include ci -> let _ = handle_for b ci ann in ()
       | T_module _ -> add_issue b Model.Error (Some ann.ann_origin) "@For on a module")
    else if fqn_eq fqn f_if_need then
      (match target with
       | T_include ci -> handle_if_need b ci ann
       | T_module _ -> add_issue b Model.Error (Some ann.ann_origin) "@IfNeed on a module")
    else if
      fqn_eq fqn f_test_for || fqn_eq fqn f_with_test || fqn_eq fqn f_with_all_tests
    then
      add_issue b Model.Error (Some ann.ann_origin)
        (Printf.sprintf "Unsupported annotation '@%s' in build inference" ann.ann_type.at_name)
    else ())

(* ------------------------------------------------------------------ *)
(* Module instance allocation                                          *)
(* ------------------------------------------------------------------ *)

let new_instance () =
  {
    Model.mi_type = Reg.dummy_module;
    mi_include_member = None;
    mi_depends = [];
    mi_no_runtime_depends = [];
    mi_options = [];
  }

let show_inst_origin (inst : Model.module_instance) =
  match inst.mi_include_member with
  | Some ci ->
      Printf.sprintf "%s(%s)" (Model.fqn_to_string inst.mi_type.mt_fqn)
        (Model.show_origin ci.ci_origin)
  | None ->
      Printf.sprintf "%s(As dependence)" (Model.fqn_to_string inst.mi_type.mt_fqn)

(** Allocate (or look up) the instance for [mod_]. Returns [None] when a fresh
 * instance must be created, [Some inst] when an existing compatible instance is
 * reused/upgraded. Adds an error issue on incompatible hierarchy. *)
let module_instance_super b (mod_ : Model.module_type) =
  let fns = mod_.mt_fqn :: List.map mod_.mt_all_super ~f:(fun s -> s.mt_fqn) in
  let fns = List.dedup_and_sort fns ~compare:Model.fqn_compare in
  let candidates =
    unique_by_ptr (List.filter_map fns ~f:(fun f -> Hashtbl.find b.store f))
  in
  match candidates with
  | [] -> None
  | [inst] ->
      let tfqn = inst.mi_type.mt_fqn in
      let in_mod_or_supers =
        fqn_eq tfqn mod_.mt_fqn
        || List.exists mod_.mt_all_super ~f:(fun s -> fqn_eq s.mt_fqn tfqn)
      in
      let in_subs = List.exists mod_.mt_all_sub_types ~f:(fun s -> fqn_eq s.mt_fqn tfqn) in
      if in_mod_or_supers || in_subs then Some inst
      else (
        add_issue b Model.Error None
          (Printf.sprintf "Module %s extends module supertype already extended by incompatible %s"
             (Model.fqn_to_string mod_.mt_fqn)
             (show_inst_origin inst));
        Some inst)
  | _ ->
      add_issue b Model.Error None
        (Printf.sprintf "Module %s extends module supertype already extended by incompatible instances"
           (Model.fqn_to_string mod_.mt_fqn));
      Some (List.hd_exn candidates)

let set_instance_to_type b (inst : Model.module_instance) (mod_ : Model.module_type) =
  inst.mi_type <- mod_;
  List.iter (mod_ :: mod_.mt_all_super) ~f:(fun super ->
    let f = super.mt_fqn in
    if not (Hashtbl.mem b.store f) then b.store_order := f :: !(b.store_order);
    Hashtbl.set b.store ~key:f ~data:inst;
    Hashtbl.set b.by_name ~key:f ~data:inst);
  List.iter mod_.mt_provides ~f:(fun provide ->
    List.iter (provide :: provide.ft_all_sub_features) ~f:(fun feat ->
      let f = feat.ft_fqn in
      let lst = Option.value ~default:[] (Hashtbl.find b.active_features f) in
      Hashtbl.set b.active_features ~key:f ~data:(lst @ [ inst ])))

let module_instance b (mod_ : Model.module_type) =
  match module_instance_super b mod_ with
  | None ->
      let inst = new_instance () in
      set_instance_to_type b inst mod_;
      inst
  | Some inst ->
      if Model.is_super_type_of inst.mi_type mod_ then set_instance_to_type b inst mod_;
      inst

(* ------------------------------------------------------------------ *)
(* Inclusion                                                           *)
(* ------------------------------------------------------------------ *)

let include_module b (mod_ : Model.module_type) =
  match Hashtbl.find b.store mod_.mt_fqn with
  | Some _ -> module_instance b mod_
  | None ->
      annotation_process b (T_module mod_);
      let inst = module_instance b mod_ in
      List.iter !(b.current_builders) ~f:(fun builder -> builder b [ inst ]);
      inst

let include_mandatory b =
  List.iter (Reg.all_module_types b.reg) ~f:(fun mt ->
    if
      List.exists mt.mt_annotations ~f:(fun a -> fqn_eq a.ann_type.at_fqn f_mandatory)
    then (
      annotation_process b (T_module mt);
      let _ = module_instance b mt in ()))

let include_all_build_modules b (cfg : Model.configuration) =
  List.iter cfg.cfg_includes ~f:(fun ci ->
    annotation_process b (T_include ci);
    annotation_process b (T_module ci.ci_module);
    let inst = module_instance b ci.ci_module in
    inst.mi_include_member <- Some ci)

(* ------------------------------------------------------------------ *)
(* Builders                                                            *)
(* ------------------------------------------------------------------ *)

let has_annotation anns fqn =
  List.exists anns ~f:(fun (a : Model.annotation) -> fqn_eq a.ann_type.at_fqn fqn)

let instance_closure b (inst : Model.module_instance) =
  let mod_ = inst.mi_type in
  List.iter mod_.mt_depends ~f:(fun dm ->
    let dep_insts = List.map dm.dep_modules ~f:(fun m -> include_module b m) in
    inst.mi_depends <- inst.mi_depends @ dep_insts;
    if has_annotation dm.dep_annotations f_no_runtime then
      let nr =
        List.filter_map dm.dep_modules ~f:(fun m -> Hashtbl.find b.store m.mt_fqn)
      in
      inst.mi_no_runtime_depends <- inst.mi_no_runtime_depends @ nr)

let close_instances b (insts : Model.module_instance list) = List.iter insts ~f:(fun inst -> instance_closure b inst)

let specify_instances b (insts : Model.module_instance list) =
  List.iter insts ~f:(fun inst ->
    match Hashtbl.find b.recommendations inst.mi_type.mt_fqn with
    | None -> ()
    | Some rec_mt ->
        if Model.is_subtype_of rec_mt inst.mi_type then (
          let _ = include_module b rec_mt in ()))

(* ------------------------------------------------------------------ *)
(* Checkers                                                            *)
(* ------------------------------------------------------------------ *)

let rec check_abstract_realization b (insts : Model.module_instance list) =
  List.iter insts ~f:(fun inst ->
    let inst_type = inst.mi_type in
    if not inst_type.mt_is_abstract then ()
    else
      (match inst_type.mt_sub_types with
       | [s] ->
           let pre_fqn = inst_type.mt_fqn in
           let _ = module_instance b s in
           (* Re-check the (possibly upgraded) instance for nested abstracts. *)
           check_abstract_realization b [ inst ];
           if inst.mi_type.mt_is_abstract then
             add_issue b Model.Error None
               (Printf.sprintf "No abstract realization: %s" (Model.fqn_to_string pre_fqn))
       | _ ->
           add_issue b Model.Error None
             (Printf.sprintf "No abstract realization: %s"
                (Model.fqn_to_string inst_type.mt_fqn))))

let check_feature_realization b (insts : Model.module_instance list) =
  List.iter insts ~f:(fun inst ->
    List.iter inst.mi_type.mt_requires ~f:(fun req ->
      match Hashtbl.find b.active_features req.ft_fqn with
      | Some lst when not (List.is_empty lst) -> ()
      | _ ->
          add_issue b Model.Error None
            (Printf.sprintf "Feature %s required by %s is not implemented"
               (Model.fqn_to_string req.ft_fqn)
               (Model.fqn_to_string inst.mi_type.mt_fqn))))

(** Shell-style integer comparison of two raw lexemes, mirroring the legacy
 * [if [ ! A -CMP B ]] test. Returns true when the constraint holds. *)
let shell_int_compare a cmp bnd =
  let af = Float.of_string a in
  let bf = Float.of_string bnd in
  (match cmp with
   | "ge" -> Float.(af >= bf)
   | "gt" -> Float.(af > bf)
   | "le" -> Float.(af <= bf)
   | "lt" -> Float.(af < bf)
   | _ -> true)

let container_fqn = function
  | Model.C_module mt -> mt.mt_fqn
  | Model.C_annotation at -> at.at_fqn

(** Numeric constraint check: for each bound number option, enforce any
 * @NumConstraint(gt/lt/ge/le) declared on the option. Not used in the scr5_32
 * build, but ported for completeness. *)
let option_check_constraints b (insts : Model.module_instance list) =
  let num_constraint = [ "mybuild"; "lang"; "NumConstraint" ] in
  List.iter insts ~f:(fun inst ->
    List.iter inst.mi_options ~f:(fun oi ->
      match oi.oi_option.opt_kind, oi.oi_value with
      | Number, V_number { lexeme; _ } ->
           List.iter oi.oi_option.opt_annotations ~f:(fun (ann : Model.annotation) ->
             if fqn_eq ann.ann_type.at_fqn num_constraint then
               (match ann.ann_arg with
                | Some arg ->
                    List.iter [ "ge"; "gt"; "le"; "lt" ] ~f:(fun cmp ->
                      match Model.ann_param arg cmp with
                      | Some (V_number { lexeme = bound; _ }) ->
                          if not (shell_int_compare lexeme cmp bound) then
                            add_issue b Model.Error None
                              (Printf.sprintf "option %s constraint check error"
                                 (Model.fqn_to_string
                                    (container_fqn oi.oi_option.opt_container)))
                      | _ -> ())
                | None -> ()))
      | _ -> ()))

(* ------------------------------------------------------------------ *)
(* Option binding                                                      *)
(* ------------------------------------------------------------------ *)

let validate_value (opt : Model.option_def) (value : Model.value) =
  match opt.opt_kind, value with
  | String, V_string _ -> true
  | Number, V_number _ -> true
  | Boolean, V_bool _ -> true
  | TypeRef, V_ref _ | TypeRef, V_module _ -> true
  | _ -> false

let find_opt_value (opt : Model.option_def) bindings =
  List.find_map bindings ~f:(fun (name, value) ->
    if String.equal name opt.opt_name && validate_value opt value then Some value else None)

let option_bind b (insts : Model.module_instance list) =
  List.iter insts ~f:(fun inst ->
    List.iter inst.mi_type.mt_all_options ~f:(fun opt ->
      let value =
        let from_binding =
          match inst.mi_include_member with
          | Some ci -> find_opt_value opt ci.ci_bindings
          | None -> None
        in
        match from_binding with
        | Some v -> Some v
        | None -> opt.opt_default
      in
      match value with
      | Some v -> inst.mi_options <- inst.mi_options @ [{ Model.oi_option = opt; oi_value = v }]
      | None ->
          add_issue b Model.Warning None
            (Printf.sprintf "Could not bind option %s in module %s to a value" opt.opt_name
               (Model.fqn_to_string inst.mi_type.mt_fqn))))

(* ------------------------------------------------------------------ *)
(* Entry point                                                         *)
(* ------------------------------------------------------------------ *)

let list_build_modules b =
  (* store_order is prepended, so it holds keys in REVERSE insertion order.
   * The legacy listBuildModules iterates the store keys in insertion order and
   * keeps the first occurrence of each instance (selectUnique). Reverse to
   * match that. *)
  let seen = ref [] in
  List.filter_map (List.rev !(b.store_order)) ~f:(fun f ->
    match Hashtbl.find b.store f with
    | None -> None
    | Some inst ->
        if List.mem !(seen) inst ~equal:(fun a c -> phys_equal a c) then None
        else (
          seen := inst :: !(seen);
          Some inst))

let check b (insts : Model.module_instance list) =
  check_abstract_realization b insts;
  check_feature_realization b insts;
  option_check_constraints b insts

let build ~reg ~cfg =
  let b = create ~reg in
  include_mandatory b;
  include_all_build_modules b cfg;
  (* Builders: activate, then apply to a snapshot. Newly added instances are
   * re-processed by all active builders (see include_module). *)
  b.current_builders := !(b.current_builders) @ [ close_instances ];
  close_instances b (list_build_modules b);
  b.current_builders := !(b.current_builders) @ [ specify_instances ];
  specify_instances b (list_build_modules b);
  (* Checkers. *)
  check b (list_build_modules b);
  (* Finalize. *)
  let modules = list_build_modules b in
  option_bind b modules;
  {
    Model.bd_configuration = cfg;
    bd_modules = modules;
    bd_issues = List.rev !(b.issues);
  }

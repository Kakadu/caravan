(* Generation of gen/src/framework/mod/mod_info.h.
 *
 * Byte-compatible (goal) with the legacy GNU Make emitter (mk/script/
 * mod-info.mk). The file assigns every module a runlevel, a priority, a
 * name and a package, in an order driven by runlevel and the runtime
 * dependency graph (topologically sorted).
 *
 * The legacy orders the new modules of each runlevel with
 *   echo <edges> | tsort | awk -f mk/tsortdiag.awk | tac
 * where tsort breaks ties alphabetically by node label. We reproduce a
 * topological sort with the same tie-breaking (alphabetical by module id)
 * in OCaml, which is deterministic and needs no subprocess.
 *
 * Written in Base (not Core) per the Core -> Base migration plan.
 *)
open Base

let id_of (inst : Model.module_instance) = Model.mi_id inst

let min_elt (lst : string list) (cmp : string -> string -> int) : string option =
  match lst with
  | [] -> None
  | hd :: tl ->
      Some (List.fold_left tl ~init:hd ~f:(fun acc x -> if cmp x acc < 0 then x else acc))

(** The runlevel of a module instance: the @Runlevel value on its config
 *  include member, or 2 when absent (legacy [mod_inst_runlevel]). *)
let runlevel_of (inst : Model.module_instance) : int =
  match inst.mi_include_member with
  | Some ci ->
      (match Ctx.anns_param ci.ci_annotations Ctx.ann_runlevel "value" with
       | Some (Model.V_number { value; _ }) -> Int.of_float value
       | _ -> 2)
  | None -> 2

(** The transitive closure of [inst] over runtime dependencies (includes
 *  [inst] itself). Returns the set of module ids in the closure. *)
let closure_ids (inst : Model.module_instance) : string list =
  let visited = Hashtbl.create (module String) in
  let queue = Stdlib.Queue.create () in
  Stdlib.Queue.push inst queue;
  Hashtbl.set visited ~key:(id_of inst) ~data:();
  while not (Stdlib.Queue.is_empty queue) do
    let cur = Stdlib.Queue.pop queue in
    List.iter (Ctx.runtime_deps cur) ~f:(fun d ->
      let did = id_of d in
      if not (Hashtbl.mem visited did) then (
        Hashtbl.set visited ~key:did ~data:();
        Stdlib.Queue.push d queue))
  done;
  List.map (Hashtbl.to_alist visited) ~f:fst

(** Topological sort of [vertices] (a list of ids) where [deps_of v] gives
 *  the ids that [v] depends on. Reproduces [tsort | tac]: dependencies
 *  come before their dependents, and ties are broken alphabetically by id
 *  (larger id first in the final order, matching the [tac] reversal of
 *  tsort's alphabetical preference). *)
let topsort (vertices : string list) (deps_of : string -> string list) : string list =
  let vset = Set.of_list (module String) vertices in
  (* predecessors[v] = the vertices that must come before v (i.e. v's deps
   * that are also vertices), self-loops ignored. *)
  let preds = Hashtbl.create (module String) in
  List.iter vertices ~f:(fun v ->
    let ps =
      List.filter (deps_of v) ~f:(fun u -> Set.mem vset u && not (String.equal u v))
    in
    Hashtbl.set preds ~key:v ~data:ps);
  let remaining = Hashtbl.create (module String) in
  List.iter vertices ~f:(fun v -> Hashtbl.set remaining ~key:v ~data:());
  let out = ref [] in
  let n = List.length vertices in
  for _ = 1 to n do
    (* pick the alphabetically smallest vertex whose predecessors are all
     * already emitted. *)
    let ready =
      List.filter vertices ~f:(fun v ->
        Hashtbl.mem remaining v
        && List.for_all (Hashtbl.find_exn preds v) ~f:(fun p -> not (Hashtbl.mem remaining p)))
    in
    match min_elt ready String.compare with
    | None -> () (* cycle: stop, matching the legacy's non-fatal behaviour *)
    | Some v ->
        out := v :: !out;
        Hashtbl.remove remaining v
  done;
  List.rev !out

(** Render mod_info.h. *)
let render (b : Model.build) : string =
  let modules = b.bd_modules in
  let id_to_inst =
    let h = Hashtbl.create (module String) in
    List.iter modules ~f:(fun m -> Hashtbl.set h ~key:(id_of m) ~data:m);
    h
  in
  let deps_of id =
    List.map (Ctx.runtime_deps (Hashtbl.find_exn id_to_inst id)) ~f:id_of
  in

  let lines = ref [] in
  let add s = lines := s :: !lines in
  let add_blank () = add "" in

  add ("/* " ^ Ctx.banner ^ " */");
  add_blank ();
  add "#ifndef GEN_SRC_FRAMEWORK_MOD_INFO_H_";
  add "#define GEN_SRC_FRAMEWORK_MOD_INFO_H_";
  add_blank ();

  let loaded_ids = ref [] in
  for r = 0 to 4 do
    let rl_modules = List.filter modules ~f:(fun m -> runlevel_of m = r) in
    let closures = List.concat (List.map rl_modules ~f:closure_ids) in
    let vertices = List.dedup_and_sort closures ~compare:String.compare in
    let sorted = topsort vertices deps_of in
    let new_ids =
      List.filter sorted ~f:(fun id -> not (List.mem !loaded_ids id ~equal:String.equal))
    in
    if not (List.is_empty new_ids) then (
      add "/* Module runlevels */";
      List.iter new_ids ~f:(fun id ->
        add ("#define __mod_runlevel__" ^ id ^ " " ^ Int.to_string r)));
    loaded_ids := !loaded_ids @ new_ids
  done;

  let loaded = List.map !loaded_ids ~f:(fun id -> Hashtbl.find_exn id_to_inst id) in

  add_blank ();
  add "/* Module priorities */";
  let prio_idx = ref 0 in
  List.iter loaded ~f:(fun inst ->
    add ("#define __mod_priority__" ^ id_of inst ^ " 1" ^ Int.to_string !prio_idx);
    prio_idx := !prio_idx + 1);
  add_blank ();

  add "/* Module names */";
  List.iter loaded ~f:(fun inst ->
    let fqn = inst.mi_type.mt_fqn in
    add ("#define __mod_name__" ^ id_of inst ^ " \"" ^ List.last_exn fqn ^ "\""));
  add_blank ();

  add "/* Module packages */";
  List.iter loaded ~f:(fun inst ->
    let fqn = inst.mi_type.mt_fqn in
    add ("#define __mod_package__" ^ id_of inst ^ " \"" ^ Model.fqn_to_string (List.drop_last_exn fqn) ^ "\""));
  add_blank ();

  add "/* Number of modules */";
  add ("#define __MOD_COUNT " ^ Int.to_string (List.length loaded));
  add_blank ();

  add "#endif /* GEN_SRC_FRAMEWORK_MOD_INFO_H_ */";
  add_blank ();

  String.concat ~sep:"\n" (List.rev !lines) ^ "\n"

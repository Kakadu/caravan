let readdir_r
  ?(cond =
    fun x ->
      let _ = x in
      true)
  directory
  =
  let handler = Unix.opendir directory in
  let rec helper handler root acc =
    let entry =
      (try Some (Unix.readdir handler) with End_of_file -> None)
    in
    match entry with
    | Some ent ->
      let x = Base.String.concat ~sep:"/" [ root; ent ] in
      let xs =
        if Filename.basename x <> "." && Filename.basename x <> ".." && Sys.is_directory x
        then (
          (* let () = Printf.printf "%S is a dir\n" (Filename.basename x) in *)
          let dir = Unix.opendir x in
          let xs = helper dir x acc in
          let () = Unix.closedir dir in
          xs)
        else []
      in
      (* let () = if cond x then Printf.printf "%s\n" x in *)
      let xxs = Base.List.filter ~f:cond (x :: xs) in
      (*error is there*)
      helper handler root (xxs @ acc)
    | None -> acc
  in
  helper handler directory []
;;

(* let _ =
   List.iter
   ~f:(fun path -> Printf.printf "%s\n" path)
   (readdir_r
   ~cond:(fun x ->
   match Filename.split_extension x with
   | _, Some ext -> String.( = ) ext "c"
   | _, _ -> false)
   directory)
   in
   let _ =
   List.iter
   ~f:(fun path -> Printf.printf "%s\n" path)
   (readdir_r
   ~cond:(fun x -> String.( = ) (Filename.basename x) "md5sums2.c")
   directory)
   in *)

let find ?(name = None) directory =
  match name with
  | Some cond ->
    let command = Printf.sprintf "find %s -name %s" directory cond in
    let ic = Unix.open_process_in command in
    In_channel.input_lines ic
  | None ->
    let command = Printf.sprintf "find %s" directory in
    let ic = Unix.open_process_in command in
    In_channel.input_lines ic
;;

(* let%expect_test "find_test1" =
   let directory =
   "/home/cy/Desktop/ocaml-rep/caravan/test/backend_test_local/canon_build/base/gen/module/embox/kernel/task/resource"
   in
   let () = Core.List.iter ~f:(fun line -> Printf.printf "%s\n" line) (find directory) in
   [%expect
    {|
    /home/cy/Desktop/ocaml-rep/caravan/test/backend_test_local/canon_build/base/gen/module/embox/kernel/task/resource
    /home/cy/Desktop/ocaml-rep/caravan/test/backend_test_local/canon_build/base/gen/module/embox/kernel/task/resource/mmap_trivial.ld_rule.mk
    /home/cy/Desktop/ocaml-rep/caravan/test/backend_test_local/canon_build/base/gen/module/embox/kernel/task/resource/umask.ld_rule.mk
    /home/cy/Desktop/ocaml-rep/caravan/test/backend_test_local/canon_build/base/gen/module/embox/kernel/task/resource/vfs.ld_rule.mk
    /home/cy/Desktop/ocaml-rep/caravan/test/backend_test_local/canon_build/base/gen/module/embox/kernel/task/resource/vfork.ld_rule.mk
    /home/cy/Desktop/ocaml-rep/caravan/test/backend_test_local/canon_build/base/gen/module/embox/kernel/task/resource/env.ld_rule.mk
    /home/cy/Desktop/ocaml-rep/caravan/test/backend_test_local/canon_build/base/gen/module/embox/kernel/task/resource/task_heap.ld_rule.mk
    /home/cy/Desktop/ocaml-rep/caravan/test/backend_test_local/canon_build/base/gen/module/embox/kernel/task/resource/argv.ld_rule.mk
    /home/cy/Desktop/ocaml-rep/caravan/test/backend_test_local/canon_build/base/gen/module/embox/kernel/task/resource/atexit.ld_rule.mk
    /home/cy/Desktop/ocaml-rep/caravan/test/backend_test_local/canon_build/base/gen/module/embox/kernel/task/resource/thread_key_table.ld_rule.mk
    /home/cy/Desktop/ocaml-rep/caravan/test/backend_test_local/canon_build/base/gen/module/embox/kernel/task/resource/module_ptr.ld_rule.mk
    /home/cy/Desktop/ocaml-rep/caravan/test/backend_test_local/canon_build/base/gen/module/embox/kernel/task/resource/sig_table.ld_rule.mk
    /home/cy/Desktop/ocaml-rep/caravan/test/backend_test_local/canon_build/base/gen/module/embox/kernel/task/resource/idesc_table.ld_rule.mk
    /home/cy/Desktop/ocaml-rep/caravan/test/backend_test_local/canon_build/base/gen/module/embox/kernel/task/resource/waitpid.ld_rule.mk
    /home/cy/Desktop/ocaml-rep/caravan/test/backend_test_local/canon_build/base/gen/module/embox/kernel/task/resource/errno.ld_rule.mk
    /home/cy/Desktop/ocaml-rep/caravan/test/backend_test_local/canon_build/base/gen/module/embox/kernel/task/resource/u_area.ld_rule.mk |}]
   ;;

   let%expect_test "find_test2" =
   let directory =
   "/home/cy/Desktop/ocaml-rep/caravan/test/backend_test_local/canon_build/base/gen"
   in
   let () =
   Core.List.iter
   ~f:(fun line -> Printf.printf "%s\n" line)
   (find ~name:(Some "md5sums1.c") directory)
   in
   [%expect
    {| /home/cy/Desktop/ocaml-rep/caravan/test/backend_test_local/canon_build/base/gen/md5sums1.c |}]
   ;; *)

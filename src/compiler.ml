(* let () = print_endline "Hello, World!" *)
(* type config = { embox_root : string } *)

let compile ~source ~filename =
  match Mybuild_parser.Parselib.parse_with_err ~source ~filename with
  | Some value -> Genlib.gen [ value ]
  | None -> ()
;;

let loop filename is_dir is_verbose () =
  let open Base in
  let compile_one filename =
    let ic = In_channel.open_text filename in
    let _ = compile ~source:(In_channel.input_all ic) ~filename in
    In_channel.close ic
  in
  let compile_many directory =
    let _ = directory in
    let open Stuff in
    let _ = readdir_r in
    let myfiles =
      find ~name:(Some "*.my") directory @ find ~name:(Some "Mybuild") directory
    in
    let () = Log.myfiles is_verbose myfiles in
    let () = Log.int is_verbose (List.length myfiles) in
    let _ = List.iter ~f:compile_one myfiles in
    ()
  in
  if is_dir then compile_many filename else compile_one filename
;;

let () =
  (* The legacy CLI was Core.Command.basic_spec with one positional argument
   * plus -dir and -v flags; this Base has no Command module, so parse argv
   * directly. *)
  let argv = Sys.argv in
  let filename = ref None in
  let is_dir = ref false in
  let is_verbose = ref false in
  let usage () =
    Out_channel.output_string Stdlib.stderr "usage: caravan [-dir] [-v] <filename>\n";
    Stdlib.exit 1
  in
  for i = 1 to Array.length argv - 1 do
    match argv.(i) with
    | "-dir" -> is_dir := true
    | "-v" -> is_verbose := true
    | f ->
        (match !filename with
         | None -> filename := Some f
         | Some _ -> usage ())
  done;
  (match !filename with
   | Some f -> loop f !is_dir !is_verbose ()
   | None -> usage ())
;;

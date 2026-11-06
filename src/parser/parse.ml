open Base

let print_value_to channel value =
  Out_channel.output_string channel (Mybuild_parser.Ast.show_myfile value ^ "\n")
;;

let parse_and_print ?(channel = Out_channel.stdout) () ~source ~filename =
  match Mybuild_parser.Parselib.parse_with_err ~source ~filename with
  | Some value -> print_value_to channel value
  (* parse_and_print lexbuf *)
  | None -> ()
;;

let loop filename () =
  let ic = In_channel.open_text filename in
  let () = parse_and_print () ~source:(In_channel.input_all ic) ~filename in
  In_channel.close ic
;;

let () =
  (* The legacy CLI was a single positional argument (Core.Command.basic_spec);
   * this Base has no Command module, so parse argv directly. *)
  let argv = Sys.get_argv () in
  if Array.length argv < 2 then (
    Out_channel.output_string Stdlib.stderr "usage: parse <filename>\n";
    Stdlib.exit 1)
  else loop argv.(1) ()
;;

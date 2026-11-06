open Base
open Lexing

type source = string

(* type 'a parse_result =
   | Failed of string
   | Parsed of 'a

   type 'a parser = source -> 'a parse_result

   let f = Parser.myfile_opt Lexer.read *)

let position_string lexbuf =
  let pos = lexbuf.lex_curr_p in
  Stdlib.Format.asprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
;;

let print_position channel lexbuf =
  Out_channel.output_string channel (position_string lexbuf)
;;

let parse_with_err ~source ~filename =
  let lexbuf = Lexing.from_string source in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try Parser.myfile_opt Lexer.read lexbuf with
  | Lexer.Syntax_error msg ->
    Out_channel.output_string Stdlib.stderr (position_string lexbuf ^ ": " ^ msg ^ "\n");
    None
  | Parser.Error ->
    Out_channel.output_string Stdlib.stderr (position_string lexbuf ^ ": Syntax error\n");
    Stdlib.exit (-1)
;;

let parse_workspace_with_err ~input_filename_list =
  List.map
    ~f:(fun (source, filename) -> parse_with_err ~source ~filename)
    input_filename_list
;;

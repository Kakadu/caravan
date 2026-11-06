(* Debug: dump build.mk text and parsed values. *)
open Base

let () =
  let root = "/home/kakadu/asp/embox-rukaml" in
  let text = Buildplan.build_mk_text root in
  Stdlib.output_string Stdlib.stderr ("TEXT LEN: " ^ Int.to_string (String.length text) ^ "\n");
  let lines = String.split text ~on:'\n' in
  List.iter lines ~f:(fun l -> Stdlib.output_string Stdlib.stderr ("LINE: [" ^ l ^ "]\n"));
  let bm = Buildplan.parse_build_mk text in
  List.iter bm ~f:(fun (k, v) -> Stdlib.output_string Stdlib.stderr ("KV: " ^ k ^ " = [" ^ v ^ "]\n"))

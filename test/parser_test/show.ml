open! Mybuild_parser.Ast

let%expect_test _ =
  let tree : myfile = Some [ "lang_test"; "p" ], [], [] in
  Printf.printf "%s\n" (show_myfile tree);
  [%expect {| ((Some ["lang_test"; "p"]), [], []) |}]
;;

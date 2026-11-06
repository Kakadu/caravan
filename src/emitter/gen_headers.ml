(* Emit all config/module headers into the given output directory.
 *
 * Usage: gen_headers.exe <root> <out_dir>
 *
 * <root>    repository root (contains Mybuild files and conf/)
 * <out_dir> directory standing in for build/base/gen
 *)
open Base

let () =
  let argv = Sys.get_argv () in
  if Array.length argv < 3 then (
    Stdlib.output_string Stdlib.stderr
      ("usage: " ^ argv.(0) ^ " <root> <out_dir>\n");
    Stdlib.exit 2);
  let root = argv.(1) in
  let out_dir = argv.(2) in
  let t = Registry.load ~root in
  let cfg = Registry.load_config t "conf/mods.conf" in
  let b = Build.build ~reg:t ~cfg:cfg in
  let ctx = { Ctx.root; out_dir; build = b } in
  let n = Headers.gen_all ctx in
  Stdlib.Format.printf "generated %d header pairs\n%!" n

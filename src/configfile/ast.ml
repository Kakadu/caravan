(* AST of Embox configuration files (mods.conf). *)

type value =
  | Str of string
  | Num of { lexeme : string; value : float }
  | Bool of bool
  | Ref of string list
[@@deriving show { with_path = false }]

type annotation_arg =
  | Params of (string * value) list
  | Value of value
[@@deriving show { with_path = false }]

type annotation = string list * annotation_arg option
[@@deriving show { with_path = false }]

type annotations = annotation list [@@deriving show { with_path = false }]

type binding = string * value [@@deriving show { with_path = false }]
type bindings = binding list [@@deriving show { with_path = false }]

type include_member = {
  module_ref : string list;
  bindings : bindings;
  annotations : annotations;
}
[@@deriving show { with_path = false }]

type configuration = {
  name : string;
  includes : include_member list;
  annotations : annotations;
}
[@@deriving show { with_path = false }]

type config_file = {
  package : string list option;
  imports : string list list;
  configurations : configuration list;
}
[@@deriving show { with_path = false }]

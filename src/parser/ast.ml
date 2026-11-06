(* Identifier *)
type ident = string [@@deriving show { with_path = false }]

(* Extended identifiers *)
type composite_name = ident list [@@deriving show { with_path = false }]
type composite_name_with_wildcard = composite_name [@@deriving show { with_path = false }]
(* type idents = ident list [@@deriving show {with_path = false} ] *)

type value =
  | StringLiteral of string
  | NumberLiteral of { lexeme : string; value : float }
  | BooleanLiteral of bool
  | Refer of composite_name
[@@deriving show { with_path = false }]

(* Annotation *)
type annotation_arg =
  | Params of (ident * value) list
  | Value of value
[@@deriving show { with_path = false }]

type annotation = composite_name * annotation_arg option
[@@deriving show { with_path = false }]

type annotations = annotation list [@@deriving show { with_path = false }]

(* Module type*)
type file_name = string [@@deriving show { with_path = false }]
type file_names = file_name list [@@deriving show { with_path = false }]

type definition_type =
  | StringType
  | NumberType
  | BooleanType
  | ReferType of composite_name
[@@deriving show { with_path = false }]

type definition = definition_type * ident * value option
[@@deriving show { with_path = false }]

type composite_names = composite_name list [@@deriving show { with_path = false }]

type module_member =
  | Depends of composite_names
  | Provides of composite_names
  | Requires of composite_names
  | Source of file_names
  | Object of file_names
  | Opt of definition
[@@deriving show { with_path = false }]

type module_members = (annotations (*option*) * module_member) list
[@@deriving show { with_path = false }]

type super_module = composite_name [@@deriving show { with_path = false }]

type module_modifier =
  | Static
  | Abstract
[@@deriving show { with_path = false }]

type module_modifiers = module_modifier list [@@deriving show { with_path = false }]

type module_type =
  module_modifiers (*option*) * ident * super_module option * module_members (*option*)
[@@deriving show { with_path = false }]

(* Interface *)
type super_features = composite_names [@@deriving show { with_path = false }]
type feature = ident * super_features option [@@deriving show { with_path = false }]

type features = (annotations (*option*) * feature) list
[@@deriving show { with_path = false }]

type super_interfaces = composite_names [@@deriving show { with_path = false }]

type interface_type = ident * super_interfaces option * features (*option*)
[@@deriving show { with_path = false }]

(* Annotation type*)
type annotation_members = (annotations (*option*) * definition) list
[@@deriving show { with_path = false }]

type annotation_type = ident * annotation_members
(*option*) [@@deriving show { with_path = false }]

type entity_type =
  | Module of module_type
  | Interface of interface_type
  | Annotation of annotation_type
[@@deriving show { with_path = false }]

type entity = annotations (*option*) * entity_type [@@deriving show { with_path = false }]
type entities = entity list [@@deriving show { with_path = false }]
type import = composite_name_with_wildcard [@@deriving show { with_path = false }]
type imports = import list [@@deriving show { with_path = false }]
type package = composite_name [@@deriving show { with_path = false }]

type myfile = package option * imports (*option*) * entities
(*option*) [@@deriving show { with_path = false }]

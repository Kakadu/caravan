%{
    open Ast
%}
%token <string> ID
%token <string> STR
%token <string * float> NUM
%token TRUE
%token FALSE
%token PERIOD
%token PERIOD_WC
%token EQ
%token COMMA
%token LEFT_PAREN
%token RIGHT_PAREN
%token AT
%token LEFT_BRACE
%token RIGHT_BRACE
%token PACKAGE_KW
%token IMPORT_KW
%token CONFIGURATION_KW
%token INCLUDE_KW
%token EOF
%start <config_file option> config_file_opt
%%

config_file_opt:
    | package = package?; imports = import*; configs = configuration+; EOF { Some { package; imports; configurations = configs } }
    | EOF { None };
package:
    | PACKAGE_KW; qualified_name = qualified_name { qualified_name };
import:
    | IMPORT_KW; qualified_name_with_wildcard = qualified_name_with_wildcard { qualified_name_with_wildcard };
configuration:
    | annotations = annotation*; CONFIGURATION_KW; name = ID; LEFT_BRACE; includes = include_member*; RIGHT_BRACE
      { { name; includes = List.concat includes; annotations } };
include_member:
    | annotations = annotation*; INCLUDE_KW; refs = separated_nonempty_list(COMMA, reference_with_initializer)
      { List.map (fun (ref, binds) ->
            { module_ref = ref;
              bindings = Option.value ~default:[] binds;
              annotations }) refs };
reference_with_initializer:
    | ref = qualified_name; init = init_args? { ref, init };
init_args:
    | LEFT_PAREN; params = parameters; RIGHT_PAREN { params };
parameters:
    | params = separated_nonempty_list(COMMA, parameter) { params };
parameter:
    | name = ID; EQ; value = value { name, value };
value:
    | str = STR { Str str }
    | num = NUM { Num { lexeme = fst num; value = snd num } }
    | TRUE { Bool true }
    | FALSE { Bool false }
    | reference = qualified_name { Ref reference };
annotation:
    | AT; reference = qualified_name; annotation_arg = annotation_arg { reference, annotation_arg };
annotation_arg:
    | LEFT_PAREN; parameters = parameters; RIGHT_PAREN { Some (Params parameters) }
    | LEFT_PAREN; value = value; RIGHT_PAREN { Some (Value value) }
    | { None };
qualified_name:
    | qualified_name = separated_nonempty_list(PERIOD, ID) { qualified_name };
qualified_name_with_wildcard:
    | qualified_name = qualified_name; PERIOD_WC { qualified_name }
    | qualified_name = qualified_name { qualified_name };

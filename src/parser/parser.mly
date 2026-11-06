%{
    open Ast
%}
%token <string> ID
%token <string> STR
%token <string * float> NUM
%token TRUE
%token FALSE
// 
%token PERIOD
%token PERIOD_WC
%token EQ
%token COMMA
%token LEFT_PAREN
%token RIGHT_PAREN
%token AT
%token LEFT_BRACE
%token RIGHT_BRACE
// 
%token STRING_KW
%token NUMBER_KW
%token BOOLEAN_KW
%token DEPENDS_KW
%token PROVIDES_KW
%token REQUIRES_KW
%token SOURCE_KW
%token OBJECT_KW
%token OPTION_KW
%token EXTENDS_KW
%token STATIC_KW
%token ABSTRACT_KW
%token MODULE_KW
%token FEATURE_KW
%token INTERFACE_KW
%token ANNOTATION_KW
%token IMPORT_KW
%token PACKAGE_KW 
%token EOF
%start <myfile option> myfile_opt
%%

myfile_opt:
    | package = package?; imports = import*; entities = entity*; EOF { Some (package, imports, entities) }
    | EOF { None };
package:
    | PACKAGE_KW; qualified_name = qualified_name { qualified_name };
    // | { None };
// imports_option: 
//     | imports = imports { Some imports }
//     | { None };
// imports: 
//     | import = import; imports = imports { import :: imports }
//     | import = import { [import] };
import: 
    | IMPORT_KW; qualified_name_with_wildcard = qualified_name_with_wildcard { qualified_name_with_wildcard };
// entities_option: 
//     | { None }
//     | entities = entities { Some entities };
// entities: 
//     // | list = annotated_type+ { list };
//     | entity = annotated_type; entities = entities { entity :: entities }
//     | entity = annotated_type { [entity] };
entity: 
    | annotations = annotation*; entity_type = entity_type { annotations, entity_type };
entity_type: 
    | module_type = module_type { Module module_type }
    | interface_type = interface_type { Interface interface_type }
    | annotation_type = annotation_type { Annotation annotation_type };
// 
// 
// 
annotation_type: 
    | ANNOTATION_KW; id = ID; LEFT_BRACE; annotation_members = annotation_member*; RIGHT_BRACE { id, annotation_members };
// annotation_members_option:
//     | annotation_members = annotation_members { Some annotation_members }
//     | { None };
// annotation_members:
//     | annotated_annotation_member = annotated_annotation_member; annotation_members = annotation_members { annotated_annotation_member :: annotation_members }
//     | annotated_annotation_member = annotated_annotation_member { [annotated_annotation_member] };
annotation_member:
    | annotations = annotation*; definition = definition { annotations, definition };
// 
// 
// 
interface_type:
    | INTERFACE_KW; id = ID; super_interfaces = super_interfaces?; LEFT_BRACE; features = feature*; RIGHT_BRACE { id, super_interfaces, features };
super_interfaces:
    | EXTENDS_KW; references = references { references };
    // | { None };
// features_option:
//     | features = features { Some features }
//     | { None };
// features:
//     | annotated_feature = annotated_feature; features = features { annotated_feature :: features }
//     | annotated_feature = annotated_feature { [annotated_feature] };
// annotated_feature:
//     | annotations = annotation*; feature = feature { annotations, feature };
feature:
    | annotations = annotation*; FEATURE_KW; id = ID; super_features = super_features? { annotations, (id, super_features) };
super_features:
    | EXTENDS_KW; references = references { references };
    // | { None };
// 
// 
// 
module_type: 
    | module_modifiers = module_modifier*; MODULE_KW; id = ID; super_module = super_module?; LEFT_BRACE; annotated_module_members = annotated_module_member*; RIGHT_BRACE { module_modifiers, id, super_module, annotated_module_members };
// module_modifiers_option:
//     | module_modifiers = module_modifiers { Some module_modifiers }
//     | { None };
// module_modifiers:
//     | module_modifier = module_modifier; module_modifiers = module_modifiers { module_modifier :: module_modifiers }
//     | module_modifier = module_modifier { [module_modifier] };
module_modifier:
    | STATIC_KW { Static }
    | ABSTRACT_KW { Abstract };
super_module:
    | EXTENDS_KW; reference = reference { reference };
    // | { None };
// module_members_option:
//     | module_members = module_members { Some module_members }
//     | { None };
// module_members:
//     | annotated_module_member = annotated_module_member; module_members = module_members { annotated_module_member :: module_members }
//     | annotated_module_member = annotated_module_member { [annotated_module_member] };
annotated_module_member:
    | annotations = annotation*; module_member = module_member { annotations, module_member };
module_member:
    | DEPENDS_KW; references = references { Depends references }
    | PROVIDES_KW; references = references { Provides references }
    | REQUIRES_KW; references = references { Requires references }
    | SOURCE_KW; filenames = filenames { Source filenames }
    | OBJECT_KW; filenames = filenames { Object filenames }
    | OPTION_KW; definition = definition { Opt definition };
references:
    | references = separated_nonempty_list(COMMA, reference) { references }
    // | reference = reference; COMMA; reference_list = reference_list { reference :: reference_list }
    // | reference = reference { [reference] };
definition:
    | definition_type = definition_type; id = ID; definition_default_value = definition_default_value? { definition_type, id, definition_default_value };
definition_type:
    | STRING_KW { StringType }
    | NUMBER_KW { NumberType }
    | BOOLEAN_KW { BooleanType }
    | reference = reference { ReferType reference };
definition_default_value:
    | EQ; value = value { value };
    // | { None };
filenames:
    | filenames = separated_nonempty_list(COMMA, filename) { filenames }
    // | filename = filename; COMMA; filename_list = filename_list { filename :: filename_list }
    // | filename = filename { [filename] };
filename:
    | str = STR { str };
// 
// 
// 
// annotations_option: 
//     | annotations = annotations { Some annotations }
//     | { None };
// annotations: 
//     | annotation = annotation; annotations = annotations { annotation :: annotations }
//     | annotation = annotation { [annotation] };
annotation:
    | AT; reference = reference; annotation_arg = annotation_arg { reference, annotation_arg };
annotation_arg:
    | LEFT_PAREN; parameters = parameters; RIGHT_PAREN { Some (Params parameters) }
    | LEFT_PAREN; value = value; RIGHT_PAREN { Some (Value value) }
    | { None };
// 
// 
//
parameters:
    | parameters = separated_nonempty_list(COMMA, parameter) { parameters }
    // | parameter = parameter; COMMA; parameters_list = parameters_list { parameter :: parameters_list }
    // | parameter = parameter { [parameter] };
parameter:
    | simple_reference = simple_reference; EQ; value = value { simple_reference, value };
value:
    | str = STR { StringLiteral str }
    | num = NUM { NumberLiteral { lexeme = fst num; value = snd num } }
    | TRUE { BooleanLiteral (true) }
    | FALSE { BooleanLiteral (false) }
    | reference = reference { Refer reference };
// 
// 
// 
reference:
    | reference = qualified_name { reference };
simple_reference:
    | simple_reference = ID { simple_reference };
qualified_name:
    | qualified_name = separated_nonempty_list(PERIOD, ID) { qualified_name };
    // | id = ID; PERIOD; qualified_name = qualified_name { id :: qualified_name }
    // | id = ID { [id] };
qualified_name_with_wildcard:
    | qualified_name = qualified_name; PERIOD_WC { qualified_name }
    | qualified_name = qualified_name { qualified_name };
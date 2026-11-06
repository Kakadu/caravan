{
    open Lexing
    open Parser

    exception Syntax_error of string
    let show_codes str = String.fold_left (fun acc x -> Int.to_string (Char.code x) ^ " " ^ acc) "" str
}

let digit = ['0'-'9']
let frac = '.' digit+
(* let printable = [ ' '-'~' ] *)
let id_head = ['a'-'z' 'A'-'Z' '_']
let id_tail = ['a'-'z' 'A'-'Z' '0'-'9' '_']
(*  *)
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
(*  *)
let str_chars = [' '-'!' '#'-'[' ']'-'~' '\xA0' '\t']
let multilevel_str_chars = [' '-'&' '('-'[' ']'-'~' '\xA0' '\t']
(*  *)
let singleline_comment_chars = [' '-'~' '\t']
(* let non_asterisk = [' '-')' '+'-'~' '\t'] *)
let non_asterisk = singleline_comment_chars # '*'
(* let non_slash = [' '-'.' '0'-'~' '\t'] *)
let non_slash = singleline_comment_chars # '/'
(*  *)
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let identifier = id_head id_tail*
let string_literal =  '"' str_chars* '"'
let number_literal = '-'? digit+ frac? | '-'? "0x" hex_digit+ frac? 
(* TODO: "-"? *)

rule read =
    parse 
    | whitespace        { read lexbuf }
    | newline           { new_line lexbuf; read lexbuf }
    | number_literal    { NUM (Lexing.lexeme lexbuf, float_of_string (Lexing.lexeme lexbuf)) }
    | "//"              { read_singleline_comment (Buffer.create 255) lexbuf; read lexbuf }
    | "/*"              { read_multiline_comment (Buffer.create 255) lexbuf; read lexbuf }
    | "true"            { TRUE }
    | "false"           { FALSE }
    | ".*"              { PERIOD_WC }
    | '.'               { PERIOD }
    | '='               { EQ }
    | ','               { COMMA }
    | '('               { LEFT_PAREN }
    | ')'               { RIGHT_PAREN }
    | '@'               { AT }
    | '{'               { LEFT_BRACE }
    | '}'               { RIGHT_BRACE }
    | "string"          { STRING_KW }
    | "number"          { NUMBER_KW }
    | "boolean"         { BOOLEAN_KW }
    | "depends"         { DEPENDS_KW }
    | "provides"        { PROVIDES_KW }
    | "requires"        { REQUIRES_KW }
    | "source"          { SOURCE_KW }
    | "object"          { OBJECT_KW }
    | "option"          { OPTION_KW }
    | "extends"         { EXTENDS_KW }
    | "static"          { STATIC_KW }
    | "abstract"        { ABSTRACT_KW }
    | "module"          { MODULE_KW }
    | "feature"         { FEATURE_KW }
    | "interface"       { INTERFACE_KW }
    | "annotation"      { ANNOTATION_KW }
    | "import"          { IMPORT_KW }
    | "package"         { PACKAGE_KW }
    | "'''"             { read_multiline_string (Buffer.create 255) lexbuf}
    | identifier        { ID (Lexing.lexeme lexbuf) }
    | '"'               { read_string (Buffer.create 255) lexbuf }
    | _                 { raise (Syntax_error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
    | eof               { EOF }
and read_singleline_comment buf =
    parse
    | newline                   {}
    | singleline_comment_chars  { read_singleline_comment buf lexbuf }
    | _                         { raise (Syntax_error ("Illegal singleline comment character: " ^ (Lexing.lexeme lexbuf))) }
    | eof                       {}
and read_multiline_comment buf = 
    parse
    | newline       { new_line lexbuf; read_multiline_comment buf lexbuf }
    | non_asterisk  { read_multiline_comment buf lexbuf }
    | '*' '/'       {} 
    (* | '*' non_slash { read_multiline_comment buf lexbuf } *)
    | '*'           { read_multiline_comment buf lexbuf }
    | _             { raise (Syntax_error ("Illegal multiline comment character: " ^ show_codes (Lexing.lexeme lexbuf))) }
    | eof           { raise (Syntax_error "Multiline comment is not terminated") }
and read_multiline_comment_asterisk buf = 
    parse
    | '/'
    | non_slash {read_multiline_comment}
and read_string buf = 
    parse
    | '"'           { STR (Buffer.contents buf) }
    | '\\' '\\'     { Buffer.add_char buf '\\'; read_string buf lexbuf }
    | '\\' '"'      { Buffer.add_char buf '\"'; read_string buf lexbuf }
    | '\\' '''      { Buffer.add_char buf '\''; read_string buf lexbuf }
    | '\\' 'n'      { Buffer.add_char buf '\n'; read_string buf lexbuf }
    | '\\' 'r'      { Buffer.add_char buf '\r'; read_string buf lexbuf }
    | '\\' 't'      { Buffer.add_char buf '\t'; read_string buf lexbuf }
    | str_chars+    { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
    | _             { raise (Syntax_error ("Illegal string character: " ^ (Lexing.lexeme lexbuf))) }
    | eof           { raise (Syntax_error "String is not terminated")}
and read_multiline_string buf =
    parse
    | "'''"                     { STR (Buffer.contents buf) }
    | "''"                      { Buffer.add_string buf "''"; read_multiline_string buf lexbuf }
    | "'"                       { Buffer.add_string buf "'"; read_multiline_string buf lexbuf }
    | newline                   { new_line lexbuf; Buffer.add_char buf '\n'; read_multiline_string buf lexbuf }
    | '\\' '\\'                 { Buffer.add_char buf '\\'; read_multiline_string buf lexbuf }
    | '\\' '''                  { Buffer.add_char buf '\''; read_multiline_string buf lexbuf }
    | '\\' 'n'                  { Buffer.add_char buf '\n'; read_multiline_string buf lexbuf }
    | '\\' 'r'                  { Buffer.add_char buf '\r'; read_multiline_string buf lexbuf }
    | '\\' 't'                  { Buffer.add_char buf '\t'; read_multiline_string buf lexbuf }
    | multilevel_str_chars+     { Buffer.add_string buf (Lexing.lexeme lexbuf); read_multiline_string buf lexbuf }
    | _                         { raise (Syntax_error ("Illegal multiline string character: " ^ show_codes (Lexing.lexeme lexbuf))) }
    | eof                       { raise (Syntax_error "Multiline string is not terminated") }
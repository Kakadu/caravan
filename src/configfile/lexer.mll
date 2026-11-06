{
    open Lexing
    open Parser

    exception Syntax_error of string
    let show_codes str = String.fold_left (fun acc x -> Int.to_string (Char.code x) ^ " " ^ acc) "" str
}

let digit = ['0'-'9']
let frac = '.' digit+
let id_head = ['a'-'z' 'A'-'Z' '_']
let id_tail = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let str_chars = [' '-'!' '#'-'[' ']'-'~' '\xA0' '\t']
let multilevel_str_chars = [' '-'&' '('-'[' ']'-'~' '\xA0' '\t']
let singleline_comment_chars = [' '-'~' '\t']
let non_asterisk = singleline_comment_chars # '*'
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let identifier = id_head id_tail*
let number_literal = '-'? digit+ frac? | '-'? "0x" hex_digit+ frac?

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
    | "package"         { PACKAGE_KW }
    | "import"          { IMPORT_KW }
    | "configuration"   { CONFIGURATION_KW }
    | "include"         { INCLUDE_KW }
    | "'''"             { read_multiline_string (Buffer.create 255) lexbuf }
    | identifier        { ID (Lexing.lexeme lexbuf) }
    | '"'               { read_string (Buffer.create 255) lexbuf }
    | _                 { raise (Syntax_error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
    | eof               { EOF }
and read_singleline_comment buf =
    parse
    | newline                   { }
    | singleline_comment_chars  { read_singleline_comment buf lexbuf }
    | _                         { raise (Syntax_error ("Illegal singleline comment character: " ^ (Lexing.lexeme lexbuf))) }
    | eof                       { }
and read_multiline_comment buf =
    parse
    | newline       { new_line lexbuf; read_multiline_comment buf lexbuf }
    | non_asterisk  { read_multiline_comment buf lexbuf }
    | '*' '/'       { }
    | '*'           { read_multiline_comment buf lexbuf }
    | _             { raise (Syntax_error ("Illegal multiline comment character: " ^ (show_codes (Lexing.lexeme lexbuf)))) }
    | eof           { raise (Syntax_error "Multiline comment is not terminated") }
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
    | eof           { raise (Syntax_error "String is not terminated") }
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
    | _                         { raise (Syntax_error ("Illegal multiline string character: " ^ (show_codes (Lexing.lexeme lexbuf)))) }
    | eof                       { raise (Syntax_error "Multiline string is not terminated") }

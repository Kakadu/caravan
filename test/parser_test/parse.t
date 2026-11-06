  $ cat << EOF > package_imports.my 
  > package lang_test.parser_test
  > import lang_test.includes.*
  > import lang_test.includes2.concrete_tool
  > EOF
  $ dune exec ../../src/parser/parse.exe ./package_imports.my
  ((Some ["lang_test"; "parser_test"]),
   [["lang_test"; "includes"]; ["lang_test"; "includes2"; "concrete_tool"]], 
   [])

  $ cat << EOF > package_annotationtype_no_members.my 
  > package lang_test.parser_test
  > annotation ann1 { }
  > EOF
  $ dune exec ../../src/parser/parse.exe ./package_annotationtype_no_members.my
  ((Some ["lang_test"; "parser_test"]), [], [([], (Annotation ("ann1", [])))])

  $ cat << EOF > package_moduletype_no_members.my 
  > package lang_test.parser_test
  > module mod1 { }
  > EOF
  $ dune exec ../../src/parser/parse.exe ./package_moduletype_no_members.my
  ((Some ["lang_test"; "parser_test"]), [],
   [([], (Module ([], "mod1", None, [])))])

  $ cat << EOF > package_interfacetype_no_members.my 
  > package lang_test.parser_test
  > interface inter1 { }
  > EOF
  $ dune exec ../../src/parser/parse.exe ./package_interfacetype_no_members.my
  ((Some ["lang_test"; "parser_test"]), [],
   [([], (Interface ("inter1", None, [])))])

  $ cat << EOF > package_entities_with_members.my 
  > package lang_test.parser_test
  > import lang_test.includes.*
  > annotation ann2 
  > {
  >     string str1
  >     number num1 = 7
  >     number num2 = 8.0
  >     number num3 = -1
  >     number hex1 = 0xF1A9
  >     number hex2 = -0xa0
  > }
  > EOF
  $ dune exec ../../src/parser/parse.exe ./package_entities_with_members.my
  ((Some ["lang_test"; "parser_test"]), [["lang_test"; "includes"]],
   [([],
     (Annotation
        ("ann2",
         [([], (StringType, "str1", None));
           ([],
            (NumberType, "num1",
             (Some NumberLiteral {lexeme = "7"; value = 7.})));
           ([],
            (NumberType, "num2",
             (Some NumberLiteral {lexeme = "8.0"; value = 8.})));
           ([],
            (NumberType, "num3",
             (Some NumberLiteral {lexeme = "-1"; value = -1.})));
           ([],
            (NumberType, "hex1",
             (Some NumberLiteral {lexeme = "0xF1A9"; value = 61865.})));
           ([],
            (NumberType, "hex2",
             (Some NumberLiteral {lexeme = "-0xa0"; value = -160.})))
           ])))
     ])

  $ cat << EOF > string_test.my
  > package lang_test.parser_test
  > module testModule1 {
  >   @ann1 source "\\\\\t\n\r\"\'"
  >   @ann2('''
  > 		AUTHORS
  > 			'Cy'
  > 			''
  > 			\\\\
  > 			\'
  > 			\t\r\n
  > 	''' )
  >   source "*.c"
  > }
  > EOF
  $ dune exec ../../src/parser/parse.exe ./string_test.my
  ((Some ["lang_test"; "parser_test"]), [],
   [([],
     (Module
        ([], "testModule1", None,
         [([(["ann1"], None)], (Source ["\\\t\n\r\"'"]));
           ([(["ann2"],
              (Some (Value
                       (StringLiteral
                          "\n\t\tAUTHORS\n\t\t\t'Cy'\n\t\t\t''\n\t\t\t\\\n\t\t\t'\n\t\t\t\t\r\n\n\t"))))
              ],
            (Source ["*.c"]))
           ])))
     ])

  $ cat << EOF > comment_test.my
  > package lang_test.parser_test
  > module comment_module {
  >   option number num //
  >   // This is singleline comment
  >   depends lang_test.deps
  >   /**/ /* This is singleline comment */
  >   source "*.c"
  >   /* This
  >      is
  >      a
  >      multiline
  >      comment
  >   */
  > }
  > EOF
  $ dune exec ../../src/parser/parse.exe ./comment_test.my
  ((Some ["lang_test"; "parser_test"]), [],
   [([],
     (Module
        ([], "comment_module", None,
         [([], (Opt (NumberType, "num", None)));
           ([], (Depends [["lang_test"; "deps"]])); ([], (Source ["*.c"]))])))
     ])

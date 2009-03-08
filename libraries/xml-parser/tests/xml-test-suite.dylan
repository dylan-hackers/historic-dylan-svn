Module: xml-test-suite
Author: Carl Gay
Synopsis: Tests for the xml-parser library

//---------------------------------------------------
// Tests for the simple-xml module
//---------------------------------------------------

define suite simple-xml-test-suite ()
  test test-with-xml;
end;

// This is here as a reminder that this test suite gives with-xml a pass
// even though it doesn't preserve alphabetic case.  Set to #t to see the
// failures.
define constant $check-alphabetic-case :: <boolean> = #f;

define test test-with-xml ()
  local method assert-equal(title, xml, string)
          // remove newlines
          let xml-string = choose(curry(\~=, '\n'), as(<string>, xml));
          if (~$check-alphabetic-case)
            xml-string := as-lowercase(xml-string);
            string := as-lowercase(string);
          end;
          check-equal(title, xml-string, string);
        end;
  assert-equal("plain element x gives <x/>",
               as(<string>, with-xml() x end),
               "<x/>");
  assert-equal("element x with empty attributes",
               as(<string>, with-xml() x () end),
               "<x/>");
  assert-equal("element x with some attributes",
               as(<string>, with-xml() x (a => "b") end),
               "<x a=\"b\"/>");
  assert-equal("element with simple body",
               as(<string>, with-xml() x { y } end),
               "<x><y/></x>");
  assert-equal("element with attributes and body",
               as(<string>, with-xml() x (a => "b") { y } end),
               "<x a=\"b\"><y/></x>");
  assert-equal("do/collect",
               with-xml()
                 x { do(collect(make(<element>, name: "FooBar"))) }
               end,
               "<x><FooBar/></x>");
  assert-equal("character entity conversion",
               with-xml() x { text("<&>") } end,
               "<x>&lt;&amp;&gt;</x>");
end test test-with-xml;



//---------------------------------------------------
// XML parsing tests
//---------------------------------------------------

define suite parsing-test-suite ()
  test test-basic-parsing;
  test test-string-parser-error-handling;
  test test-stream-parser-error-handling;
end;

define test test-basic-parsing ()
  for (text in list("<x/>"))
    check-no-errors(format-to-string("%s parses without error", text),
                    parse-document(text));
  end;
end;

define constant $broken-xml
  = list(#("<x>", "no end element"),
         #("<x></y>", "end element doesn't match"),
         #("<", "start element incomplete #1"),
         #("<x", "start element incomplete #2"),
         #("<x a=>", "incomplete attribute def #1"),
         #("<x a=\">", "incomplete attribute def #2")
         // ...
         );

define test test-string-parser-error-handling ()
  for (item in $broken-xml)
    let (text, reason) = apply(values, item);
    check-condition(concatenate("Meta parser: ", reason),
                    <xml-parse-error>,
                    parse-document(text));
  end;
end test test-string-parser-error-handling;

define test test-stream-parser-error-handling ()
  for (item in $broken-xml)
    let (text, reason) = apply(values, item);
    check-condition(concatenate("Stream parser: ", reason),
                    <xml-parse-error>,
                    parse(make(<xml-stream-parser>,
                               stream: make(<string-stream>,
                                            contents: text))));
  end;
end test test-stream-parser-error-handling;

//---------------------------------------------------
// XML transform tests
//---------------------------------------------------

define suite transform-test-suite ()
  test basic-transform-test;
end;

define test basic-transform-test ()
  let text = "<x><y a=\"1\" b=\"2\">text1</y>text2</x>";

  local method parents-are-set? (node :: <element>, parent)
          element-parent(node) = parent
            & every?(method (child)
                       ~instance?(child, <element>)
                         | parents-are-set?(child, node)
                     end,
                     node-children(node))
        end;
  let document = parse-document(text);
  check-true("parse-documents sets parent nodes (via <add-parents>)",
             parents-are-set?(document.root, document.root.element-parent));

  check-no-errors("default transform shouldn't err",
                  transform(parse-document(text),
                            make(<null-transform>)));

end test basic-transform-test;

// <xform-state> is abstract so we need this.
define class <null-transform> (<xform-state>)
end;


//---------------------------------------------------
// Main
//---------------------------------------------------

define suite xml-test-suite ()
  suite simple-xml-test-suite;
  suite transform-test-suite;
  suite parsing-test-suite;
end;

define method main () => ()
  let filename = locator-name(as(<file-locator>, application-name()));
  if (split(filename, ".")[0] = "xml-test-suite")
    run-test-application(xml-test-suite);
  end;
end method main;

begin
  main()
end;


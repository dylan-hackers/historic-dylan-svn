Module:    xml-test
Synopsis:  Exercises the XML parser lib
Author:    Douglas M. Auclair
Copyright: (c) 2001, LGPL
Version:   1.0

define function display(function, string)
  let (a, b) = function(string, start: 0, end: string.size);
  format-out("%=, %=\n", a, b);
  force-output(*standard-output*);
end;

define function test-name-stuff()
  format-out("\nName stuff:\n");
  display(parse-namechar, "a");
  display(parse-namechar, "s");
  display(parse-namechar, "b");
  display(parse-namechar, "1");
  display(parse-namechar, "aa");
  display(parse-name, "chris");
  display(parse-name, "chrXis");
  display(parse-char-data, "abcd");
end function test-name-stuff;

define function test-ref-stuff()
  format-out("\nref stuff\n");
  display(parse-char-ref, "&#123;");
  display(parse-char-ref, "&#x0f;");  
  display(parse-entity-ref, "&nbsp;");  
  display(parse-entity-ref, "&nbsp");  
  display(parse-reference, "&nbsp;");
  display(parse-reference, "&#123;");
  display(parse-reference, "&#x0e;");
end function test-ref-stuff;

define function test-attribute-stuff()
  format-out("\nattribute stuff\n");
  display(parse-att-value, "\"123\"");
  display(parse-att-value, "\"&#x0e;\"");
  display(parse-attribute, "name=\"Douglas M. Auclair\"");
  display(parse-attribute, "abcd=\"&#x0e;\"");
  display(parse-attribute, "abcd = \"&#x0e;\"");
  display(parse-attribute, "abcd = &#x0e;");
end function test-attribute-stuff;

define function test-versioning-stuff()
  format-out("\nversioning stuff:\n");
  display(parse-version-num, "1.0");
  display(parse-version-info, " version=\"1.0\"");
  let decl =  "<?xml version=\"1.0\"?>\n";
  display(parse-xml-decl, decl);
end function test-versioning-stuff;

define function test-element-stuff()
  format-out("\nElement stuff:\n");
/** This stuff works:
  display(parse-empty-elem-tag, "<one/>");
  display(parse-empty-elem-tag, "<one />");
  display(parse-s-tag, "<two>");
  display(parse-s-tag, "<three >");
  display(parse-s-tag, "<four xyz=\"12\">");
  display(parse-s-tag, "<five xyz=\"12\" abc=\"&nbsp;\" >");
  display(parse-s-tag, "<five xyz=\"12\" abc=\"&nbsp\" >");
 **/
  display(parse-content, "<one>hi there</one>");
  display(parse-content, "<one></one>");
  display(parse-content, "<one/>");
  display(parse-content, "<one><two>sdf</two></one>");
  display(parse-content, "<one><two>sdf</two><three>sdf</three></one>");
  display(parse-element, "<foo>bar</foo>");
  display(parse-element, "<foo/>");
  display(parse-element, "<foo><bar/><baz>quux</baz></foo>");
end function test-element-stuff;

define function test-document-stuff()
  let doctype = "<!DOCTYPE foo SYSTEM \"foo.dtd\">";
  display(parse-doctype, doctype);

  format-out("\ndocument stuff:\n");
/** This stuff works:
  display(parse-comment, "<!-- dfgfggf -->");
  display(parse-pi-target, "xml-stylesheet");
 **/

  let style =  "<?xml-stylesheet href=\"servers.xsl\" type=\"text/xsl\"?>";
  display(parse-pi, "<? xmlversion1.0 ?>");
  display(parse-pi, style);
  display(parse-misc, style);
  display(parse-prolog, decl);
  display(parse-prolog, concatenate(decl, style));
  display(parse-prolog, concatenate(decl, style, doctype));

  display(parse-document, "<?xml version=\"1\"?><one><two>sdf</two></one>");
  let(index, doc) = parse-document("<?xml version=\"1\"?><one><two>sdf</two></one>");
  display-node(doc);
  display(parse-document, "<?xml version=1 ?><one><two>sdf</two></one>");
end function test-document-stuff;

define method test () => ()
/** This all works:
  test-name-stuff();
***/
  test-ref-stuff();
  test-attribute-stuff();
  test-versioning-stuff();
  test-element-stuff();
  test-document-stuff();

end method test;

define method main(file-name :: <string>)
  test();
/*****
  with-open-file(fs = file-name)
    let (index, document) = parse-document(fs.stream-contents);
    display-node(document);
  end;
 *****/
end method main;

begin
  format-out("Start...\n");
  main(application-arguments()[0]);
  format-out("...End\n");
end;


Module:    xml-test
Synopsis:  Exercises the XML library
Author:    Douglas M. Auclair, doug@cotilliongroup.com
Copyright: (c) 2001, LGPL
Version:   1.0

define constant $testable-fns = make(<table>);

define class <my-xml-builder> (<xml-builder>)
end class <my-xml-builder>;

define variable *my-builder* = make(<my-xml-builder>);

define method start-element (builder :: <my-xml-builder>, element :: <xml-element>)
  format-out("<%s", element.name);
  for(value keyed-by key in element.attributes)
    format-out(" %s='%s'", key, value);
  end for;
  format-out(">\n");
end method start-element;

define method end-element (builder :: <my-xml-builder>, element :: <xml-element>)
  format-out("</%s>\n", element.name);
end method end-element;

define function main(name, arguments)
  let fn = $testable-fns[as(<symbol>, arguments[0])];
  let index = 0;
  let xml-string = arguments[1];
  format-out("Trying %s on [%s]\n", arguments[0], arguments[1]);
  while(index < xml-string.size)
    let (index1, #rest tag) = fn(xml-string, start: index);
    format-out("index: %= value: %=\n", index1, tag);
    index := index1;
  end while;
  exit-application(0);
end function main;

// Invoke our main() function.
begin
  $testable-fns[#"attribute"] := parse-attribute;
  $testable-fns[#"xml-element-start"] 
    := curry(parse-xml-element-start, *my-builder*);
  $testable-fns[#"char-data"] := parse-char-data;
  $testable-fns[#"stag"] := parse-stag;
  $testable-fns[#"empty-elem-tag"] := parse-empty-elem-tag;
  $testable-fns[#"element"] := parse-element;
  $testable-fns[#"comment"] := parse-comment;
  $testable-fns[#"etag"] := parse-etag;
  $testable-fns[#"version-num"] := parse-version-num;
  $testable-fns[#"system-literal"] := parse-system-literal;
  $testable-fns[#"pubid-literal"] := parse-pubid-literal;
  $testable-fns[#"cd-sect"] := parse-cd-sect;
  $testable-fns[#"elementdecl"] := parse-elementdecl;

  main(application-name(), application-arguments());
end;


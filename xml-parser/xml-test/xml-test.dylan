Module:    xml-test
Synopsis:  Exercises the XML library
Author:    Douglas M. Auclair, doug@cotilliongroup.com
Copyright: (c) 2001, LGPL
Version:   1.0

define constant $testable-fns = make(<table>);

define method do-the-rest(sym :: <symbol>, str :: <string>)
  let fn = $testable-fns[sym];
  let index = 0;
  let xml-string = str;
  format-out("Trying %s on [%s]\n", sym, str);
  while(index < xml-string.size)
    let (index1, #rest tag) = fn(xml-string, start: index);
    format-out("index: %= value: %=\n", index1, tag);
    index := index1;
  end while;
end method do-the-rest;

define method do-the-rest(sym == #"file", str :: <string>)
  with-open-file(in = str)
    let (index, doc) = parse-document(in.stream-contents);
    format-out("Doug's doc:\n%s\n", transform-document(doc));
   // display-node(doc);
  end;
end method do-the-rest;

define function main(name, arguments)
  let sym = as(<symbol>, arguments[0]);
  do-the-rest(sym, arguments[1]);
  exit-application(0);
end function main;

// Invoke our main() function.
begin
  $testable-fns[#"attribute"] := parse-attribute;
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
  $testable-fns[#"char-ref"] := parse-char-ref;
  $testable-fns[#"content"] := parse-content;
  $testable-fns[#"pi"] := parse-pi;
  $testable-fns[#"ent"] := parse-def|content;
  
  main(application-name(), application-arguments());
end;


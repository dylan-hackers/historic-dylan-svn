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
    let doc = parse-document(in.stream-contents);
    transform-document(doc, state: #"html");
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

//-------------------------------------------------------
// Here's an example of tranforming XML to XML readable 
// as HTML
define variable *ent* = make(<table>);

define method before-transform(state == #"html", times :: <integer>,
                               str :: <stream>)
  write(str, "\n<BR>");
  for(x from 1 to times)
    write(str, "&nbsp;");
  end for;
end method before-transform;

define method transform(in :: <document>, tag-name :: <symbol>,
    state == #"html", str :: <stream>)
  *ent* := make(<table>);
  write(str, "<HTML>\n <TITLE>XML as HTML</TITLE>\n <BODY>\n");
  next-method();
  write(str, "<HR>Referenced entities:<P>\n");
  referenced-entities(str);
  write(str, "\n </BODY>\n</HTML>");
end method transform;

define method transform(in :: <element>, tag-name :: <symbol>,
                        state == #"html", str :: <stream>)
  write(str, concatenate("&lt;<FONT COLOR='green'>", 
                         as(<string>, tag-name), "</FONT>"));
  for(x in in.element-attributes)
    transform(x, x.name, #"html", str);
  end for;
  write(str, "&gt;");
  next-method();
  before-transform(#"html", *depth*, str);
  write(str, format-to-string("&lt;<FONT COLOR='green'>"
                              "/%s</FONT>&gt;",
                              as(<string>, tag-name)));
end method transform;

define method transform(in :: <attribute>, tag-name :: <symbol>,
                        state == #"html", str :: <stream>)
  write(str, format-to-string(" <FONT COLOR='blue'>%s</FONT>='%s'", 
                              as(<string>, tag-name),
                              in.attribute-value));
end method transform;

define method transform(in :: <entity-reference>, tag-name :: <symbol>,
                        state == #"html", str :: <stream>)
  write(str, 
    format-to-string("<A HREF='#%s'>&amp;%s;</A>",
                     as(<string>, tag-name), as(<string>, tag-name)));
  *ent*[tag-name] := in.entity-value;
end method transform;

// turn all < to &lt;, > to &gt; and & to &amp;
define method transform(in :: <char-reference>, tag-name :: <symbol>,
                        state == #"html", str :: <stream>)
  write(str, check-char(in.char));
end method transform;

define method transform(in :: <char-string>, tag-name :: <symbol>,
                        state == #"html", str :: <stream>)
  for(ch in in.text)
    write(str, check-char(ch));
  end for;
end method transform;

define function check-char(ch :: <character>) => (checked :: <string>)
  select(ch)
    '<' => "&lt;";
    '>' => "&gt;";
    '&' => "&amp;";
    otherwise => make(<string>, size: 1, fill: ch);
  end select;
end function check-char;

define function referenced-entities(str :: <stream>)
  let entities = sort(map(curry(as, <string>), *ent*.key-sequence));
  for(x in entities)
    write(str, format-to-string("<LI><STRONG><A NAME='%s'>%s</A></STRONG>: ", x, x));
    transform(make(<char-string>, text: *ent*[as(<symbol>, x)]), #"text", #"html", str);
  end for;
end function referenced-entities;


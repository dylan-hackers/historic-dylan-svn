Module:    xml-test
Synopsis:  Exercises the XML library
Author:    Douglas M. Auclair, doug@cotilliongroup.com
Copyright: (c) 2001, LGPL
Version:   1.0

define function main(name, arguments)
  with-open-file(in = arguments[0])
    let doc = parse-document(in.stream-contents);
    transform-document(doc, state: #"html");
  end;
  exit-application(0);
end function main;

// Invoke our main() function.
begin
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
  *ent*[tag-name] := in;
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
  let ent = sort(map(curry(as, <string>), *ent*.key-sequence));
  for(x in ent)
    write(str, format-to-string("<LI><STRONG><A NAME='%s'>%s</A></STRONG>: ", x, x));
    for(y in *ent*[as(<symbol>, x)].entity-value)
      transform(y, y.name, #"html", str); 
    end;
  end for;
end function referenced-entities;


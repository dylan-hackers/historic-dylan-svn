Module:    html-xform
Synopsis:  Converts XML to HTML-readable text
Author:    Douglas M. Auclair, doug@cotilliongroup.com
Copyright: (c) 2001, LGPL
Version:   1.0

//-------------------------------------------------------
// Here's an example of tranforming XML to XML readable 
// as HTML
define class <html> (<xform-state>) end;
define constant $html = make(<html>);
define variable *substitute?* :: <boolean> = #t;

define method before-transform(node :: <node>, state :: <html>,
                               times :: <integer>,
                               str :: <stream>)
  format(str, "\n<BR>");
  for(x from 1 to times)
    format(str, "&nbsp;");
  end for;
end method before-transform;

define method transform(in :: <document>, tag-name :: <symbol>,
                        state :: <html>, str :: <stream>)
  collect-entity-defs(in); 
  format(str, "<HTML>\n <TITLE>XML as HTML</TITLE>\n <BODY>\n&lt;?"
              "<FONT COLOR='green'>xml</FONT> <FONT COLOR='blue'>"
              "version</FONT>=\"1.0\"?&gt;<BR>\n");
  referenced-entities(as(<string>, tag-name), str, state); 
  next-method();
  format(str, "\n </BODY>\n</HTML>");
end method transform;

define method transform(in :: <element>, tag-name :: <symbol>,
                        state :: <html>, str :: <stream>)
  let namei = as(<string>, tag-name);
  format(str, "&lt;<FONT COLOR='green'>%s</FONT>", namei); 
  for(x in in.element-attributes)
    transform(x, x.name, state, str);
  end for;
  format(str, "&gt;");
  next-method();
  before-transform(in, state, *xml-depth*, str);
  format(str, "&lt;<FONT COLOR='green'>/%s</FONT>&gt;", namei);
end method transform;

define method transform(in :: <attribute>, tag-name :: <symbol>,
                        state :: <html>, str :: <stream>)
  format(str, " <FONT COLOR='blue'>%s</FONT>=\"%s\"", 
         as(<string>, tag-name), in.attribute-value);
end method transform;

define method transform(in :: <entity-reference>, tag-name :: <symbol>,
                        state :: <html>, str :: <stream>)
  let name = as(<string>, tag-name);
  format(str, "<A HREF='#%s'>&amp;%s;</A>", name, name);
end method transform;

// turn all < to &lt;, > to &gt; and & to &amp;
define method transform(in :: <char-reference>, tag-name :: <symbol>,
                        state :: <html>, str :: <stream>)
  format(str, "<font color='red'>&amp;%s;</font>", as(<string>, tag-name));
end method transform;

define method transform(in :: <char-string>, tag-name :: <symbol>,
                        state :: <html>, str :: <stream>)
  for(ch in in.text)
    format(str, check-char(ch));
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


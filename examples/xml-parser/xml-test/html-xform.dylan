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

define method before-transform(state :: <html>,
                               times :: <integer>,
                               str :: <stream>)
  write(str, "\n<BR>");
  for(x from 1 to times)
    write(str, "&nbsp;");
  end for;
end method before-transform;

define method transform(in :: <document>, tag-name :: <symbol>,
                        state :: <html>, str :: <stream>)
 // *ent* := make(<table>);
  collect-entity-defs(in);
  let name = as(<string>, tag-name);

  write(str, "<HTML>\n <TITLE>XML as HTML</TITLE>\n <BODY>\n");
  write(str, "&lt;?<FONT COLOR='green'>xml</FONT> "
             "<FONT COLOR='blue'>version</FONT>=\"1.0\"?&gt;<BR>\n"
             "&lt;!<FONT COLOR='purple'>DOCTYPE </FONT><FONT COLOR='green'>");
/****
  write(str, format-to-string("%s</FONT> SYSTEM \"<A HREF='%s"
                              "-dtd.html'>%s.dtd</A>\">\n<P>\n",
                              name, name, name));
***/
  write(str, concatenate(name, "</FONT> ["));
  referenced-entities(str, state);
  write(str, "\n<BR>]&gt;<BR>");
  next-method();
//  write(str, "<HR>\nReferenced entities:<P>\n");
  write(str, "\n </BODY>\n</HTML>");
end method transform;

define method transform(in :: <element>, tag-name :: <symbol>,
                        state :: <html>, str :: <stream>)
  write(str, concatenate("&lt;<FONT COLOR='green'>", 
                         as(<string>, tag-name), "</FONT>"));
  for(x in in.element-attributes)
    transform(x, x.name, state, str);
  end for;
  write(str, "&gt;");
  next-method();
  before-transform(state, *xml-depth*, str);
  write(str, format-to-string("&lt;<FONT COLOR='green'>"
                              "/%s</FONT>&gt;",
                              as(<string>, tag-name)));
end method transform;

define method transform(in :: <attribute>, tag-name :: <symbol>,
                        state :: <html>, str :: <stream>)
  write(str, format-to-string(" <FONT COLOR='blue'>%s</FONT>=\"%s\"", 
                              as(<string>, tag-name),
                              in.attribute-value));
end method transform;

define method transform(in :: <entity-reference>, tag-name :: <symbol>,
                        state :: <html>, str :: <stream>)
  write(str, 
    format-to-string("<A HREF='#%s'>&amp;%s;</A>",
                     as(<string>, tag-name), as(<string>, tag-name)));
//  *ent*[tag-name] := in;
end method transform;

// turn all < to &lt;, > to &gt; and & to &amp;
define method transform(in :: <char-reference>, tag-name :: <symbol>,
                        state :: <html>, str :: <stream>)
  write(str, concatenate("<font color='red'>&amp;", 
                          as(<string>, tag-name), ";</font>"));
end method transform;

define method transform(in :: <char-string>, tag-name :: <symbol>,
                        state :: <html>, str :: <stream>)
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


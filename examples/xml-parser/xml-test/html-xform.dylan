Module:    html-xform
Synopsis:  Converts XML to HTML-readable text
Author:    Douglas M. Auclair, doug@cotilliongroup.com
Copyright: (c) 2001, LGPL
Version:   1.0

//-------------------------------------------------------
// Here's an example of tranforming XML to XML readable 
// as HTML
define class <html> (<xform-state>) 
  class slot document-name :: <string>;
end class <html>;

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
  state.document-name := as(<string>, in.name);
  collect-entity-defs(in);
  let dtd = concatenate(state.document-name, "-entities.html");
  format(str, "<HTML>\n <TITLE>XML as HTML</TITLE>\n <BODY>\n&lt;?"
              "<FONT COLOR='green'>xml</FONT> <FONT COLOR='blue'>"
              "version</FONT>=\"1.0\"?&gt;\n<BR>\n&lt;!<FONT COLOR="
              "'purple'>DOCTYPE </FONT><FONT COLOR='green'>%s</FONT> "
              "<FONT COLOR='purple'>SYSTEM </FONT>\"<A HREF='%s'>%s.dtd"
              "</A>\"&gt;\n<P>\n<FONT COLOR='orange'>\n&lt;!-- %s.xml "
              "parsed by xml-parser, "
              "written by <A HREF='mailto://doug@cotilliongroup.com'>"
              "Douglas M. Auclair</A>, <A HREF='mailto://chris@double.co.nz'>"
              "Chris Double</A>, and <A HREF='mailto://ich@andreas.org'>"
              "Andreas Bogk</A>.\n<BR>\n&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"
              "An LGPL example application available from the <A HREF='"
              "http://www.gwydiondylan.org'>GwydionDylan</A> web site"
              " --&gt;</FONT>\n<P>\n", state.document-name, dtd, 
              state.document-name, state.document-name);
  with-open-file(file = dtd, direction: #"output")
    referenced-entities(state.document-name, file, state);
  end with-open-file;
  format-out("Wrote out %s\n", dtd);
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
  format(str, "<A HREF='%s-entities.html#%s'>&amp;%s;</A>", 
         state.document-name, name, name);
end method transform;

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

// turn all < to &lt;, > to &gt; and & to &amp;
define function check-char(ch :: <character>) => (checked :: <string>)
  select(ch)
    '<' => "&lt;";
    '>' => "&gt;";
    '&' => "&amp;";
    otherwise => make(<string>, size: 1, fill: ch);
  end select;
end function check-char;


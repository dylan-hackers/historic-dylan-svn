Module:    html-xform
Synopsis:  Converts XML to HTML-readable text
Author:    Douglas M. Auclair, doug@cotilliongroup.com
Copyright: (c) 2001, LGPL
Version:   1.0

//-------------------------------------------------------
// Here's an example of tranforming XML to XML readable 
// as HTML
define class <html> (<printing>)
  class slot document-name;
end class <html>;

define constant $html = make(<html>);
define variable *substitute?* :: <boolean> = #t;

define method before-transform(node :: <node>, state :: <html>,
                               times :: <integer>, str :: <stream>)
  format(str, "\n<BR>");
  for(x from 1 to times) format(str, "&nbsp;"); end for;
end method before-transform;

define method transform(in :: <document>, tag-name :: <symbol>,
                        state :: <html>, str :: <stream>)
  state.document-name := as(<string>, in.name);
  collect-entity-defs(in);
  let dtd = concatenate(state.document-name, "-entities.html");
  format(str, "<HTML>\n <TITLE>XML as HTML</TITLE>\n <BODY BGCOLOR='white'>\n"
              "&lt;?<FONT COLOR='green'>xml</FONT> <FONT COLOR='blue'>"
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

define function name-color(name :: <string>, color :: <string>)
 => (s :: <string>)
  format-to-string("<FONT COLOR='%s'>%s</FONT>", color, name);
end function name-color;

define method xml-name(e :: <element>, state :: <html>) => (s :: <string>)
  name-color(as(<string>, e.name), "green");
end method xml-name;

define method xml-name(a :: <attribute>, state :: <html>) => (s :: <string>)
  name-color(as(<string>, a.name), "blue");
end method xml-name;

define method xml-name(ch :: <char-reference>, state :: <html>)
 => (s :: <string>)
  name-color(next-method(), "red");
end method xml-name;

define method transform(in :: <entity-reference>, tag-name :: <symbol>,
                        state :: <html>, str :: <stream>)
  format(str, "<A HREF='%s-entities.html#%s'>", state.document-name, tag-name);
  next-method();
  format(str, "</A>");
end method transform;

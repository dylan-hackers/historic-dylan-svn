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

define method before-transform(node :: type-union(<document>, <element>),
 			       state :: <html>, times :: <integer>,
			       stream :: <stream>)
                               
  format(stream, "\n<BR>");
  for(x from 1 to times) format(stream, "&nbsp;"); end for;
end method before-transform;

define method transform(c :: <comment>, name :: <symbol>, state :: <html>,
                        stream :: <stream>)
  format(stream, "<BR><FONT color='brown'>");
  next-method();
  format(stream, "</FONT><BR>");
end method transform;

define method prepare-document(doc :: <document>, state :: <html>,
                               stream :: <stream>) => ()
  format(stream, "<HTML>\n <TITLE>XML as HTML</TITLE>\n"
           " <BODY BGCOLOR='white'>\n");
  next-method();
  let dname = as(<string>, doc.name);
  state.document-name := dname;
  collect-entity-defs(doc);
  let dtd = make(<dtd>, name: doc.name, 
                 ref: concatenate(dname, "-entities.html"));
  new-line(stream);
  print-in("purple", dtd, stream);
  format(stream, "%=", header-comment(dname));
end method prepare-document;

define method transform(in :: <document>, tag-name :: <symbol>,
                        state :: <html>, stream :: <stream>)
  next-method();

  let name = state.document-name;
  let dtd-file = concatenate(name, "-entities.html");
  with-open-file(file = dtd-file, direction: #"output")
    referenced-entities(name, file, state);
  end with-open-file;
  format-out("Wrote out %s\n", dtd-file);
  format(stream, "\n </BODY>\n</HTML>");
end method transform;

define function name-color(name :: <string>, color :: <string>)
 => (s :: <string>)
  format-to-string("<FONT COLOR='%s'>%s</FONT>", color, name);
end function name-color;

define method xml-name(e :: <tag>, state :: <html>) => (s :: <string>)
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

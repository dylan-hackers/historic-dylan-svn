Module:    entity-pass
Synopsis:  Grabs all entities from the document
Author:    Douglas M. Auclair, doug@cotilliongroup.com
Copyright: (c) 2001, LGPL
Version:   1.0

define variable *ent* = make(<table>);
define class <1st-pass> (<object>) end;

define function collect-entity-defs(in :: <document>)
  *ent* := make(<table>);
  let ignored-stream = make(<string-stream>, contents: "");
  transform-document(in, state: make(<1st-pass>),
                     stream: ignored-stream);
end function collect-entity-defs;

define function referenced-entities(namei :: <string>, 
                                    str :: <stream>,
                                    state)
  unless(*ent*.empty?)
  format(str, "<HTML>\n<BODY BGCOLOR='white'>\n<P>\n<FONT COLOR='"
              "orange'>&lt;!-- %s.dtd parsed by xml-parser, "
              "written by <A HREF='mailto://doug@cotilliongroup.com'>"
              "Douglas M. Auclair</A>, <A HREF='mailto://chris@double.co.nz'>"
              "Chris Double</A>, and <A HREF='mailto://ich@andreas.org'>"
              "Andreas Bogk</A>.\n<BR>\n&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"
              "An LGPL example application available from the <A HREF='"
              "http://www.gwydiondylan.org'>GwydionDylan</A> web site"
              " --&gt;</FONT>\n<P>\n", namei);
  let ent = sort(map(curry(as, <string>), *ent*.key-sequence));
  for(x in ent)
    format(str, "\n<BR>&nbsp;&lt;!<FONT COLOR='purple'>"
           "ENTITY</FONT> <FONT COLOR='red'><A NAME='%s'>%s</A>"
           "</FONT> '", x, x);
    for(y in *ent*[as(<symbol>, x)].entity-value)
      transform(y, y.name, state, str); 
    end;
    format(str, "'&gt;");
  end for;
  format(str, "\n</BODY>\n</HTML>");
  end unless;
end function referenced-entities;

//-------------------------------------------------------
// xform function that grabs the entities and their defs
define method transform(in :: <entity-reference>, 
                        tag-name :: <symbol>,
                        state :: <1st-pass>, 
                        str :: <stream>)
  *ent*[tag-name] := in; // got this one, 
// now let's see if it refers to any
  do(method(x) transform(x, x.name, state, str) end, in.entity-value);
end method transform;


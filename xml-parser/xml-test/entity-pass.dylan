Module:    entity-pass
Synopsis:  Grabs all entities from the document
Author:    Douglas M. Auclair, doug@cotilliongroup.com
Copyright: (c) 2001, LGPL
Version:   1.0

define variable *ent* = make(<table>);
define class <1st-pass> (<xform-state>) end;

define function collect-entity-defs(in :: <document>)
  *ent* := make(<table>);
  let ignored-stream = make(<string-stream>, contents: "");
  transform-document(in, state: make(<1st-pass>), stream: ignored-stream);
end function collect-entity-defs;

define function print-in(color :: <string>, xml :: <xml>, stream :: <stream>)
  format(stream, "<p><FONT COLOR='%s'>%=</FONT></p>\n", color, xml);
end function print-in;

define function header-comment(file-name :: <string>) => (c :: <comment>)
  make(<comment>, 
       comment: concatenate(file-name, ".xml parsed by the xml-"
                              "parser library written by Douglas M. Auclair,"
                              " Chris Double, and Andreas Bogk and available"
                              " from http://www.gwydiondylan.org."));
end function header-comment;

define function referenced-entities(namei :: <string>, str :: <stream>, state)
  unless(*ent*.empty?)
    format(str, "<HTML>\n<BODY BGCOLOR='white'>\n");
    print-in("brown", namei.header-comment, str);
    let ent = sort(map(curry(as, <string>), *ent*.key-sequence));
    for(x in ent) print-in("purple", *ent*[as(<symbol>, x)], str) end for;
    format(str, "\n</BODY>\n</HTML>");
  end unless;
end function referenced-entities;

//-------------------------------------------------------
// xform function that grabs the entities and their defs
define method transform(in :: <entity-reference>, 
                        tag-name :: <symbol>,
                        state :: <1st-pass>, 
                        str :: <stream>)
  *ent*[tag-name] := make(<internal-entity>, name: as(<string>, tag-name), 
                          expands-to: in.entity-value); // got this one, 
  // now let's see if it refers to any
  do(method(x) transform(x, x.name, state, str) end, in.entity-value);
end method transform;

Module:    entity-pass
Synopsis:  Grabs all entities from the document
Author:    Douglas M. Auclair, doug@cotilliongroup.com
Copyright: (c) 2001, LGPL
Version:   1.0

define variable *ent* = make(<table>);
define class <1st-pass> (<xform-state>) end;
define constant $entities-first = make(<1st-pass>);

define function collect-entity-defs(in :: <document>)
  *ent* := make(<table>);
  let ignored-stream = make(<string-stream>, contents: "");
  transform(in, in.name, $entities-first, ignored-stream);
end function collect-entity-defs;

define function referenced-entities(namei :: <string>, 
                                    str :: <stream>,
                                    state :: <xform-state>)
  unless(*ent*.empty?)
  format(str, "&lt;!<FONT COLOR='purple'>DOCTYPE </FONT>"
              "<FONT COLOR='green'>%s</FONT> [", namei);
  let ent = sort(map(curry(as, <string>), *ent*.key-sequence));
  for(x in ent)
    format(str, "\n<BR>&nbsp;&lt;!<FONT COLOR='purple'>"
           "ENTITY</FONT> <FONT COLOR='red'><A NAME='%s'>%s</A>"
           "</FONT> '", x, x);
    for(y in *ent*[as(<symbol>, x)].entity-value)
      transform(y, y.name, state, str); 
    end;
    format(str, "'>");
  end for;
  format(str, "\n<BR>]&gt;<BR>");
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


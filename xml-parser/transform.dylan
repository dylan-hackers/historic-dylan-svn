Module:    transform
Author:    Douglas M. Auclair
Copyright: (C) 2001, LGPL
synopsis:  provides the default way to transform XML (just gets the
           text), and a facility to allow user transformations.

define open abstract class <xform-state> (<object>) end;

define open generic transform(elt :: <xml>, state :: <xform-state>);

//  the standard (default) transformation functions
define method transform(nodes :: <document>, state :: <xform-state>)
  for(node in nodes.node-children)
    transform(node, state);
  end for;
end method transform;

define method transform(nodes :: <element>, state :: <xform-state>)
  for(node in nodes.node-children)
    transform(node, state);
  end for;
end method transform;

define method transform(tag :: <tag>, state :: <xform-state>)
  // do nothing
end method transform;

// N.B. no default xforms for attributes

define method transform(in :: <char-string>, state :: <xform-state>)
//  write(str, in.text);  // uncomment to see only the data
end method transform;

// this xform should not happen when substitute-entities == #t
define method transform(in :: <reference>, state :: <xform-state>)
  do-reference-xform(in, state);
end method transform;

// added this method to clear up ambiguities between <reference> specialization
// and <xform-state> specialization.
define method do-reference-xform(in :: <entity-reference>, state :: <xform-state>)
  for(x in in.entity-value) transform(x, state) end;
end method do-reference-xform;

define method do-reference-xform(in :: <char-reference>, state :: <xform-state>)
//  write-element(str, in.char);
end method do-reference-xform;

Module:    transform
Author:    Douglas M. Auclair
Copyright: (C) 2001, LGPL
synopsis:  provides the default way to transform XML (just gets the
           text), and a facility to allow user transformations.

define variable *xml-depth* = 0;
define open class <xform-state> (<object>) end;
define constant $no-state = make(<xform-state>);

define function transform-document(doc :: <document>, 
    #key state :: <xform-state> = $no-state, 
    stream :: <stream> = *standard-output*)
  *xml-depth* := 0;
  transform(doc, doc.name, state, stream);
end function transform-document;

define open generic transform(elt :: <xml>, tag-name :: <symbol>, 
                              state :: <xform-state>, str :: <stream>);

//  the standard (default) transformation functions
define method transform(nodes :: <node>, tag-name :: <symbol>,
                        state :: <xform-state>, str :: <stream>)
  *xml-depth* := *xml-depth* + 1;
  for(node in nodes.node-children)
    before-transform(nodes, state, *xml-depth*, str);
    transform(node, node.name, state, str);
  end for;
  *xml-depth* := *xml-depth* - 1;
end method transform;

define open generic before-transform(node :: <node>, 
    state :: <xform-state>, rep :: <integer>, str :: <stream>);
define method before-transform(node :: <node>, 
    state :: <xform-state>, rep :: <integer>, str :: <stream>) end;

define method transform(in :: <document>, tag-name :: <symbol>, 
    state :: <xform-state>, str :: <stream>)
  next-method();
end method transform;

define method transform(in :: <element>, tag-name :: <symbol>,
                        state :: <xform-state>, str :: <stream>)
  next-method();  // continue iteration over my children
end method transform;

// N.B. no default xforms for attributes

define method transform(in :: <char-string>, tag-name :: <symbol>,
                        state :: <xform-state>, str :: <stream>)
//  write(str, in.text);  // uncomment to see only the data
end method transform;

// these two xforms should not happen when substitute-entities == #t
define method transform(in :: <entity-reference>, tag-name :: <symbol>,
                        state :: <xform-state>, str :: <stream>)
  for(x in in.entity-value) transform(x, x.name, state, str) end;
end method transform;

define method transform(in :: <char-reference>, tag-name :: <symbol>,
                        state :: <xform-state>, str :: <stream>)
//  write-element(str, in.char);
end method transform;


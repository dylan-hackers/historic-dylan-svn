Module:    transform
Author:    Douglas M. Auclair
Copyright: (C) 2001, LGPL
synopsis:  provides the default way to transform XML (just gets the
           text), and a facility to allow user transformations.

define variable *depth* = 0;

define function transform-document(doc :: <document>, 
    #key state :: <symbol> = #"none", 
    stream :: <stream> = *standard-output*)
  *depth* := 0;
  transform(doc, doc.name, state, stream);
end function transform-document;

define open generic transform(elt :: <xml>, tag-name :: <symbol>, 
                              state :: <symbol>, str :: <stream>);

//  the standard (default) transformation functions
define method transform(nodes :: <node>, tag-name :: <symbol>,
                        state :: <symbol>, str :: <stream>)
  *depth* := *depth* + 1;
  for(node in nodes.node-children)
    before-transform(state, *depth*, str);
    transform(node, node.name, state, str);
  end for;
  *depth* := *depth* - 1;
end method transform;

define open generic before-transform(state :: <symbol>, 
    rep :: <integer>, str :: <stream>);
define method before-transform(state :: <symbol>, rep :: <integer>,
                               str :: <stream>) end;

define method transform(in :: <document>, tag-name :: <symbol>, 
    state :: <symbol>, str :: <stream>)
  next-method();
end method transform;

define method transform(in :: <element>, tag-name :: <symbol>,
                        state :: <symbol>, str :: <stream>)
  next-method();  // continue iteration over my children
end method transform;

// N.B. no default xforms for attributes

define method transform(in :: <char-string>, tag-name :: <symbol>,
                        state :: <symbol>, str :: <stream>)
  write(str, in.text);
end method transform;

define method transform(in :: <entity-reference>, tag-name :: <symbol>,
                        state :: <symbol>, str :: <stream>)
  write(str, in.entity-value);
end method transform;

define method transform(in :: <char-reference>, tag-name :: <symbol>,
                        state :: <symbol>, str :: <stream>)
  write-element(str, in.char);
end method transform;


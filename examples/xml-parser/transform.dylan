Module:    transform
Author:    Douglas M. Auclair
Copyright: (C) 2001, LGPL
synopsis:  provides the default way to transform XML (just gets the
           text), and a facility to allow user transformations.

define function transform-document(doc :: <document>, 
    #key state :: <symbol> = #"none") => (transformed :: <string>)
  transform(doc, doc.name, state, "");
end function transform-document;

define open generic transform(elt :: <xml>, tag-name :: <symbol>, 
                              state :: <symbol>, str :: <string>)
 => (xform :: <string>);

//  the standard (default) transformation functions
define method transform(nodes :: <node>, tag-name :: <symbol>,
                        state :: <symbol>, str :: <string>)
 => (xform :: <string>)
  let string = str;
  for(node in nodes.node-children)
    string := transform(node, node.name, state, string);
  end for;
  string;
end method transform;

define method transform(in :: <document>, tag-name :: <symbol>, 
    state == #"none", str :: <string>) => (xform :: <string>)
  next-method();
end method transform;

define method transform(in :: <element>, tag-name :: <symbol>,
                        state :: <symbol>, str :: <string>)
 => (xform :: <string>)
  next-method();  // continue iteration over my children
end method transform;

// N.B. no default xforms for attributes

define method transform(in :: <char-string>, tag-name :: <symbol>,
                        state :: <symbol>, str :: <string>)
 => (xform :: <string>)
  concatenate(str, in.text);
end method transform;

define method transform(in :: <entity-reference>, tag-name :: <symbol>,
                        state :: <symbol>, str :: <string>)
 => (xform :: <string>)
  concatenate(str, *entities*[tag-name]);
end method transform;

define method transform(in :: <char-reference>, tag-name :: <symbol>,
                        state :: <symbol>, str :: <string>)
 => (xform :: <string>)
  let new-str = concatenate(str, " ");
  new-str.last := in.char;
  new-str;
end method transform;


Module:    transform
Author:    Douglas M. Auclair
Copyright: (C) 2001, LGPL
synopsis:  provides the default way to transform XML (just gets the
           text), and a facility to allow user transformations.

define variable *xml-depth* = 0;
define open class <xform-state> (<object>) end;
define constant $no-state = make(<xform-state>);

define function transform-document(doc :: <document>, 
    #key state = $no-state,
    stream :: <stream> = *standard-output*)
  *xml-depth* := 0;
  prepare-document(doc, state, stream);
  transform(doc, doc.name, state, stream);
end function transform-document;

define open generic prepare-document(doc :: <document>, state :: <xform-state>,
				     stream :: <stream>) => ();
define method prepare-document(doc :: <document>, state :: <xform-state>,
			       stream :: <stream>) => () end;

define function node-iterator(nodes :: <node-mixin>, state :: <xform-state>,
                              stream :: <stream>)
  *xml-depth* := *xml-depth* + 1;
  for(node in nodes.node-children)
    before-transform(nodes, state, *xml-depth*, stream);
    transform(node, node.name, state, stream);
  end for;
  *xml-depth* := *xml-depth* - 1;
end function node-iterator;

define open generic transform(elt :: <xml>, tag-name :: <symbol>, 
                              state :: <xform-state>, str :: <stream>);

//  the standard (default) transformation functions
define method transform(doc :: <document>, tag-name :: <symbol>,
                        state :: <xform-state>, str :: <stream>)
  node-iterator(doc, state, str);
end method transform;

define method transform(elt :: <element>, tag-name :: <symbol>,
                        state :: <xform-state>, str :: <stream>)
  node-iterator(elt, state, str);
end method transform;

define method transform(tag :: <tag>, tag-name :: <symbol>,
			state :: <xform-state>, stream :: <stream>)
  // do nothing
end method transform;

define open generic before-transform(node :: <xml>, state :: <xform-state>, 
                                     rep :: <integer>, stream :: <stream>);
define method before-transform(node :: <xml>, state :: <xform-state>, 
                               rep :: <integer>, stream :: <stream>) end;

// N.B. no default xforms for attributes

define method transform(in :: <char-string>, tag-name :: <symbol>,
                        state :: <xform-state>, str :: <stream>)
//  write(str, in.text);  // uncomment to see only the data
end method transform;

// this xform should not happen when substitute-entities == #t
define method transform(in :: <reference>, tag-name :: <symbol>,
                        state :: <xform-state>, str :: <stream>)
  do-reference-xform(in, state, str);
end method transform;

// added this method to clear up ambiguities between <reference> specialization
// and <xform-state> specialization.
define method do-reference-xform(in :: <entity-reference>,
                                 state :: <xform-state>, str :: <stream>)
  for(x in in.entity-value) transform(x, x.name, state, str) end;
end method do-reference-xform;

define method do-reference-xform(in :: <char-reference>,
                                 state :: <xform-state>, str :: <stream>)
//  write-element(str, in.char);
end method do-reference-xform;

module: builder-impl
synopsis: Implements a META parser for XML 1.0
author: Andreas Bogk <andreas@andreas.org>, based on work by Chris Double
translated-into-a-library-by: Douglas M. Auclair, doug@cotilliongroup.com
copyright: LGPL

// implementation methods for <xml-builder> construction
// on an XML file

define method parse-xml-element-start(builder :: <xml-builder>, 
    string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c, elt, embedded-end-tag);
    [parse-beginning-of-tag(elt),
     {["/", yes!(embedded-end-tag)], []},
     ">"];
    start-element(builder, elt);
    if(embedded-end-tag)
      end-element(builder, elt);
    end if;
    values(index, elt);
  end with-meta-syntax;
end method parse-xml-element-start;

define method start-element(bldr :: <xml-builder>, elt :: <xml-element>)
  error("You must subclass <xml-builder> and add to start-element");
end method start-element;

define method end-element(bldr :: <xml-builder>, elt :: <xml-element>)
  error("You must subclass <xml-builder> and add to end-element");
end method end-element;

/**** REMOVED
define method text(bldr :: <xml-builder>, txt :: <string>)
  error("You must subclass <xml-builder> and add to text");
end method text;
*****/



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
    variables(c, element-name, attributes, space, embedded-end-tag);
    ["<",
     parse-name(element-name),
     loop(parse-s(space)),
     parse-xml-attributes(attributes),
     {["/", yes!(embedded-end-tag)], []},
     ">"];
    let tag :: <xml-element> = make(<xml-element>, 
       name: element-name, attributes: attributes);
    start-element(builder, tag);
    if(embedded-end-tag)
      end-element(builder, tag);
    end if;
    values(index, tag);
  end with-meta-syntax;
end method parse-xml-element-start;

define method start-element(bldr :: <xml-builder>, elt :: <xml-element>)
  error("You must subclass <xml-builder> and start-element");
end method start-element;

define method end-element(bldr :: <xml-builder>, elt :: <xml-element>)
  error("You must subclass <xml-builder> and end-element");
end method end-element;



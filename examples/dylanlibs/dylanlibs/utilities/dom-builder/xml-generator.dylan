Module:    xml-generator
Synopsis:  Converts a DOM to XML.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

// Print the html element to the given stream as XML.
define generic print-xml(e :: <element>, stream :: <stream>) => ();

define method print-xml(e :: <element>, stream :: <stream>) => ()
  for(child in e.element-children)
    print-xml(child, stream);
  end for;
end method print-xml;

define method print-xml(e :: <node-element>, stream :: <stream>) => ()
  let node-tag-name = string-downcase(as(<string>, e.node-element-tag));
  write-element(stream, '<');
  write(stream, node-tag-name);
  unless(empty?(e.node-element-attributes))
    write-element(stream, ' ');
    for(attribute keyed-by index in e.node-element-attributes)
      unless(index = 0)
        write(stream, " ");
      end unless;

      write(stream, string-downcase(as(<string>, attribute.attribute-key)));
      write(stream, "=\"");
      write(stream, attribute.attribute-value);
      write-element(stream, '\"');
    end for;
  end unless;

  write-element(stream, '>');
  next-method();
  write(stream, "</");
  write(stream, node-tag-name);
  write-element(stream, '>');
end method print-xml;

define method print-xml(t :: <text-element>, stream :: <stream>) => ()
  write(stream, t.text-element-text);
end method print-xml;

// Print the html element to a string.
define generic print-xml-to-string(e :: <element>) => (r :: <string>);

define method print-xml-to-string(e :: <element>) => (r :: <string>)
  with-output-to-string(stream)
    print-xml(e, stream);
  end;
end method print-xml-to-string;


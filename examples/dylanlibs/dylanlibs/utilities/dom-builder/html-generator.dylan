Module:    html-generator
Synopsis:  Converts a DOM to HTML.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

// Print the html element to the given stream as HTML.
define generic print-html(e :: <element>, stream :: <stream>) => ();

define method print-html(e :: <element>, stream :: <stream>) => ()
  for(child in e.element-children)
    print-html(child, stream);
  end for;
end method print-html;

define variable *non-closed-tags* =
    #[#"area", 
      #"base", 
      #"basefont", 
      #"bgsound", 
      #"br",
      #"button",
      #"col",
      #"colgroup",
      #"embed",
      #"hr",
      #"input",
      #"isindex",
      #"keygen",
      #"link",
      #"meta",
      #"object",
      #"plaintext",
      #"spacer",
      #"wbr"];

define method print-html(e :: <node-element>, stream :: <stream>) => ()
  let node-tag-name = as(<string>, e.node-element-tag);
  write-element(stream, '<');
  write(stream, node-tag-name);
  unless(empty?(e.node-element-attributes))
    write-element(stream, ' ');
    for(attribute keyed-by index in e.node-element-attributes)
      unless(index = 0)
        write(stream, " ");
      end unless;

      write(stream, as(<string>, attribute.attribute-key));
      write(stream, "=\"");
      write(stream, attribute.attribute-value);
      write-element(stream, '\"');
    end for;
  end unless;

  write-element(stream, '>');
  next-method();
  unless(member?(e.node-element-tag, *non-closed-tags*))
    write(stream, "</");
    write(stream, node-tag-name);
    write-element(stream, '>');
  end unless;
end method print-html;

define method print-html(t :: <text-element>, stream :: <stream>) => ()
  write(stream, t.text-element-text);
end method print-html;

// Print the html element to a string.
define generic print-html-to-string(e :: <element>) => (r :: <string>);

define method print-html-to-string(e :: <element>) => (r :: <string>)
  with-output-to-string(stream)
    print-html(e, stream);
  end;
end method print-html-to-string;


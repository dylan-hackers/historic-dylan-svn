module: printing
author: Douglas M. Auclair
copyright: (c) 2002, LGPL
synopsis: provides a simple, controllable way to print elements

// these variables provide a way to change the look of element tags in case
// e.g. they want HTML output, instead.
define variable *open-tag* :: <string> = "<";
define variable *close-tag* :: <string> = ">";
define variable *ampersand* :: <string> = "&";

// actually, this printing module depends heavily on the transform module
// (the printing module may be viewed as a derivative work) ... so pretty
// printing, etc, is controlled by the transform methods and their helpers

define open class <printing> (<xform-state>) end;
define variable *printer-state* :: <printing> = make(<printing>);

define open generic xml-name(xml :: <xml>, state :: <printing>)
 => (s :: <string>);
define method xml-name(xml :: <xml>, state :: <printing>) => (s :: <string>)
  as(<string>, xml.name);
end method xml-name;
define method xml-name(ref :: <reference>, state :: <printing>)
 => (s :: <string>)
  concatenate(*ampersand*, next-method(), ";");
end method xml-name;

// This method follows a common pattern in this module:  print-object
// calls transform to do its work.  The transform methods, therefore, 
// exercise more precise control over elements ... print-object is a
// easy-going approach to viewing elements.
define method print-object(e :: <element>, s :: <stream>) => ()
  transform(e, e.name, *printer-state*, s);
end method print-object;

// use before-transform for pretty printing elements -- see
// xml-test/html-xform for an example that uses this module as a
// pretty printer
define method transform(e :: <element>, tag-name :: <symbol>, 
                        state :: <printing>, s :: <stream>)
  format(s, "%s%s", *open-tag*, xml-name(e, state));
  for(attr in e.element-attributes) transform(attr, attr.name, state, s) end;

  // this test identifies empty elements when printing (saves a bit of space).
  if(e.text.empty? & e.node-children.empty?)
    format(s, "/%s", *close-tag*)
  else
    format(s, "%s", *close-tag*);
    next-method();  // the text is one or several of the children
    before-transform(e, state, *xml-depth*, s);
    format(s, "%s/%s%s", *open-tag*, xml-name(e, state), *close-tag*);
  end if;
end method transform;

define method print-object(a :: <attribute>, s :: <stream>) => ()
  transform(a, a.name, *printer-state*, s);
end method print-object;

define method transform(a :: <attribute>, tag-name :: <symbol>,
                        state :: <printing>, s :: <stream>)
  format(s, " %s=\"%s\"", xml-name(a, state), a.attribute-value);
end method transform;

define method transform(str :: <char-string>, tag-name :: <symbol>,
                        state :: <printing>, s :: <stream>)
  print-object(str, s);
end method transform;

define method print-object(str :: <char-string>, s :: <stream>) => ()
  for(ch in str.text)
    format(s, check-char(ch));
  end for;
end method print-object;

// turn all < to *open-tag*, > to *close-tag*; and & to *ampersand*
define function check-char(ch :: <character>) => (checked :: <string>)
  select(ch)
    '<' => *open-tag*;
    '>' => *close-tag*;
    '&' => *ampersand*;
    otherwise => make(<string>, size: 1, fill: ch);
  end select;
end function check-char;

define method transform(ref :: <reference>, tag-name :: <symbol>,
                        state :: <printing>, s :: <stream>)
  format(s, "%s", xml-name(ref, state));
end method transform;

define method print-object(ref :: <reference>, s :: <stream>) => ()
  transform(ref, ref.name, *printer-state*, s);
end method print-object;

// a document contains one <element>
define method print-object(doc :: <document>, s :: <stream>) => ()
  print-object(doc.node-children[0], s);
end method print-object;

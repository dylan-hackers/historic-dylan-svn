module: printing
author: Douglas M. Auclair
copyright: (c) 2002, LGPL
synopsis: provides a simple, controllable way to print elements

// these variables provide a way to change the look of element tags in case
// e.g. they want HTML output, instead.
define variable *open-the-tag* :: <string> = "<";
define variable *close-the-tag* :: <string> = ">";
define variable *ampersand* :: <string> = "&";

// actually, this printing module depends heavily on the transform module
// (the printing module may be viewed as a derivative work) ... so pretty
// printing, etc, is controlled by the transform methods and their helpers

define open class <printing> (<xform-state>) end;
define variable *printer-state* :: <printing> = make(<printing>);

// xml-name turns out to be a pretty powerful way of printing
// information about the xml thing being printed
define open generic xml-name(xml :: <xml>, state :: <printing>)
 => (s :: <string>);
define method xml-name(xml :: <xml>, state :: <printing>) => (s :: <string>)
  xml.name-with-proper-capitalization;
end method xml-name;
define method xml-name(ref :: <reference>, state :: <printing>)
 => (s :: <string>)
  concatenate(*ampersand*, next-method(), ";");
end method xml-name;

// This method follows a common pattern in this module:  print-object
// calls transform to do its work.  The transform methods, therefore, 
// exercise more precise control over elements ... print-object is a
// easy-going approach to viewing elements.
define method print-object(t :: <tag>, s :: <stream>) => ()
  transform(t, t.name, *printer-state*, s);
end method print-object;

define method transform(t :: <tag>, tag-name :: <symbol>, 
                        state :: <printing>, stream :: <stream>)
  print-opening(t, state, stream);
  do-transform(t, state, stream);
  print-closing(t.before-close, stream);
end method transform;

define function print-opening(t :: <tag>, state :: <printing>, 
                              stream :: <stream>)
  format(stream, "%s%s%s", *open-the-tag*, t.after-open, xml-name(t, state));
end function print-opening;

define function print-closing(back :: <string>, stream :: <stream>)
  format(stream, "%s%s", back, *close-the-tag*);
end function print-closing;

define function print-attributes(attribs :: <sequence>, state :: <printing>,
                                 stream :: <stream>) => ()
  for(attr in attribs) transform(attr, attr.name, state, stream) end;
end function print-attributes;

define generic do-transform(xml :: <xml>, state :: <printing>, 
                            stream :: <stream>) => ();
define method do-transform(pi :: <processing-instruction>, 
                           state :: <printing>, stream :: <stream>) => ()
  print-attributes(pi.attributes, state, stream);
end method do-transform;

define function quoted-printer(fn :: <function>, s :: <stream>, #key arg = "")
 => (printer :: <function>)
  method(seq :: <sequence>, state :: <printing>)
      format(s, "%s \"", arg);
      fn(seq, state, s);
      format(s, "\"");
  end method;
end function quoted-printer;

define function print-safely-quoted(text :: <string>, arg :: <string>,
                                    state :: <printing>, stream :: <stream>)
 => ()
  quoted-printer(method(string, ignored-state, strm) 
                   print-safe-string(string, strm);
                 end, stream, arg: arg) (text, state);
end function print-safely-quoted;

define function quoted-sequence(seq :: <sequence>, state :: <printing>,
                                stream :: <stream>)
  for(elt in seq)
    transform(elt, elt.name, state, stream)
  end for;
end function quoted-sequence;

define function print-system-info(sys :: <external-mixin>, state :: <printing>, 
                                  stream :: <stream>) => ()
                                  
  do-print-sys-info(sys, state, sys.sys/pub, stream);
end function print-system-info;

define generic do-print-sys-info(ext :: <external-mixin>, state :: <printing>,
				 kind :: one-of(#f, #"system", #"public"),
				 stream :: <stream>) => ();

define method do-print-sys-info(ext :: <external-mixin>, state :: <printing>,
				kind == #f, stream :: <stream>) => ()
  // Don't print anything
end method do-print-sys-info;

define method do-print-sys-info(ext :: <external-mixin>, state :: <printing>,
				kind == #"system", stream :: <stream>) => ()
  print-safely-quoted(ext.sys-id, " SYSTEM", state, stream);
end method do-print-sys-info;

define method do-print-sys-info(ext :: <external-mixin>, state :: <printing>,
				kind == #"public", stream :: <stream>) => ()
  print-safely-quoted(ext.pub-id, " PUBLIC", state, stream);
  print-safely-quoted(ext.sys-id, "", state, stream);
end method do-print-sys-info;

define method do-transform(ie :: <internal-entity>, state :: <printing>,
                           stream :: <stream>) => ()
  quoted-printer(quoted-sequence, stream)(ie.expansion, state);
end method do-transform;

define method do-transform(ee :: <external-entity>, state :: <printing>,
                           stream :: <stream>) => ()
  print-system-info(ee, state, stream);
end method do-transform;

define method do-transform(dtd :: <dtd>, state :: <printing>, 
                           stream :: <stream>) => ()
  print-system-info(dtd, state, stream);
  unless(dtd.internal-declarations.empty?)
    format(stream, " [ ");
    for(x in dtd.internal-declarations)
      transform(x, x.name, state, stream);
    end for;
    format(stream, " ]");
  end unless;
end method do-transform;

define method do-transform(c :: <comment>, state :: <printing>,
                           stream :: <stream>) => ()
  format(stream, " ");
  print-safe-string(c.comment, stream);
end method do-transform;

// use before-transform for pretty printing elements -- see
// xml-test/html-xform for an example that uses this module as a
// pretty printer
define method transform(e :: <element>, tag-name :: <symbol>,
                        state :: <printing>, stream :: <stream>)
  print-opening(e, state, stream);
  print-attributes(e.attributes, state, stream);

  // this test identifies empty elements when printing (saves a bit of space).
  if(e.text.empty? & e.node-children.empty?)
    print-closing("/", stream);
  else
    print-closing("", stream);
    // the text is one or several of the children
    node-iterator(e, state, stream);
    before-transform(e, state, *xml-depth*, stream);
    format(stream, "%s/%s%s", *open-the-tag*, xml-name(e, state), 
           *close-the-tag*);
  end if;
end method transform;

define method print-object(a :: <attribute>, s :: <stream>) => ()
  transform(a, a.name, *printer-state*, s);
end method print-object;

define method transform(a :: <attribute>, tag-name :: <symbol>,
                        state :: <printing>, s :: <stream>)
  format(s, " %s=\"%m\"", xml-name(a, state), 
         curry(print-safe-string, a.attribute-value));
end method transform;

define method transform(str :: <char-string>, tag-name :: <symbol>,
                        state :: <printing>, s :: <stream>)
  print-object(str, s);
end method transform;

define method print-object(str :: <char-string>, s :: <stream>) => ()
  print-safe-string(str.text, s);
end method print-object;

define method print-safe-string(str :: <string>, s :: <stream>) => ()
  for(ch in str)
    format(s, check-char(ch));
  end for;
end method print-safe-string;

// turn all < to *open-the-tag*, > to *close-the-tag*; and & to *ampersand*
define function check-char(ch :: <character>) => (checked :: <string>)
  select(ch)
    '<' => *open-the-tag*;
    '>' => *close-the-tag*;
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

// a document contains one <element> and pi's and comments
define method print-object(doc :: <document>, s :: <stream>) => ()
  transform(doc, doc.name, *printer-state*, s);
end method print-object;

// the default way for preparing to print a document
define method prepare-document(doc :: <document>, state :: <printing>, 
                               stream :: <stream>) => ()
end method prepare-document;

module: printing
author: Douglas M. Auclair
copyright: (c) 2002, LGPL
synopsis: provides a simple, controllable way to print elements

// actually, this printing module depends heavily on the transform module
// (the printing module may be viewed as a derivative work) ... so pretty
// printing, etc, is controlled by the transform methods and their helpers

define open class <printing> (<xform-state>) 
  constant slot stream :: <stream>, required-init-keyword: stream:;
end;

define generic xml-name(xml :: <xml>)
 => (s :: <string>);

define method xml-name(xml :: <xml>) => (s :: <string>)
  xml.name-with-proper-capitalization;
end method xml-name;

define method xml-name(ref :: <reference>)
 => (s :: <string>)
  concatenate("&", next-method(), ";");
end method xml-name;

// This method follows a common pattern in this module:  print-object
// calls transform to do its work.  The transform methods, therefore, 
// exercise more precise control over elements ... print-object is a
// easy-going approach to viewing elements.
define method print-object(t :: <tag>, s :: <stream>) => ()
  let state = make(<printing>, stream: s);
  transform(t, state);
end method print-object;

define method transform(t :: <tag>, state :: <printing>)
  let stream = state.stream;
  print-opening(t, stream);
  do-transform(t, state);
  print-closing(t, stream);
end method transform;

define function print-opening(t :: <tag>, stream :: <stream>)
  format(stream, "<%s%s", t.after-open, t.xml-name);
  pprint-indent(#"block", 2, stream);
end function print-opening;

define function print-closing(t :: <tag>, stream :: <stream>)
  format(stream, "%s>", t.before-close);
  pprint-indent(#"block", 0, stream);
end function print-closing;

define function print-attributes(attribs :: <sequence>, state :: <printing>) => ()
  for(attr in attribs) transform(attr, state) end;
end function print-attributes;

define generic do-transform(xml :: <xml>, state :: <printing>) => ();

define method do-transform(pi :: <processing-instruction>, 
                           state :: <printing>) => ()
  print-attributes(pi.attributes, state);
end method do-transform;

define function print-safely-quoted(text :: <string>, stream :: <stream>)
 => ()
  write-element(stream, ' ');
  pprint-newline(#"fill", stream);
  print-escaped-quotes(text, stream);
end function print-safely-quoted;

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
  write(stream, " SYSTEM");
  print-safely-quoted(ext.sys-id, stream);
end method do-print-sys-info;

define method do-print-sys-info(ext :: <external-mixin>, state :: <printing>,
				kind == #"public", stream :: <stream>) => ()
  write(stream, " PUBLIC");
  print-safely-quoted(ext.pub-id, stream);
  print-safely-quoted(ext.sys-id, stream);
end method do-print-sys-info;

define method do-transform(ie :: <internal-entity>, state :: <printing>) => ()
  let stream = state.stream;
  write-element(stream, ' ');
  pprint-newline(#"fill", stream);
  write-element(stream, '"');
  for(elt in ie.expansion)
    transform(elt, state)
  end for;
  write-element(stream, '"');
end method do-transform;

define method do-transform(ee :: <external-entity>, state :: <printing>) => ()
  let stream = state.stream;
  print-system-info(ee, state, stream);
end method do-transform;

define method do-transform(dtd :: <dtd>, state :: <printing>) => ()
  let stream = state.stream;
  print-system-info(dtd, state, stream);
  unless(dtd.internal-declarations.empty?)
    format(stream, " [ ");
    for(x in dtd.internal-declarations)
      transform(x, state);
    end for;
    format(stream, " ]");
  end unless;
end method do-transform;

define method do-transform(c :: <comment>, state :: <printing>) => ()
  let stream = state.stream;
  write-element(stream, ' ');
  print-filled-string(c.comment, stream);
end method do-transform;

define thread variable *started-text?* = #f;

define method transform(e :: <element>, state :: <printing>)
  let stream = state.stream;
  printing-logical-block(stream)
    print-opening(e, stream);
    print-attributes(e.attributes, state);


    // this test identifies empty elements when printing (saves a bit of space).
    if(e.text.empty? & e.node-children.empty?)
      write-element(stream, '/');
      print-closing(e, stream);
    else
      dynamic-bind (*started-text?* = 
                      *started-text?* 
                        | any?(rcurry(instance?, <char-string>), e.node-children))
        print-closing(e, stream);
        pprint-indent(#"block", 2, stream);
        unless (*started-text?*)
          pprint-newline(#"mandatory", stream);
        end;
        let first = #t;
        for(node in e.node-children)
          unless (first | *started-text?*)
            pprint-newline(#"mandatory", stream);
          end;
          first := #f;
          transform(node, state);
        end for;
        pprint-indent(#"block", 0, stream);
        unless (*started-text?*)
          pprint-newline(#"mandatory", stream);
        end;
        format(stream, "</%s>", e.xml-name);
      end;
    end;
  end;
end method transform;

define method transform(e :: type-union(<processing-instruction>, <dtd>),
                        state :: <printing>)
  let stream = state.stream;
  printing-logical-block(stream)
    next-method();
    pprint-newline(#"mandatory", stream);
  end;
end method transform;

define method print-object(a :: <attribute>, s :: <stream>) => ()
  let state = make(<printing>, stream: s);
  transform(a, state);
end method print-object;

define method transform(a :: <attribute>, state :: <printing>)
  let stream = state.stream;
  write-element(stream, ' ');
  pprint-newline(#"fill", stream);
  format(stream, 
         "%s=%m",
         a.xml-name, 
         curry(print-escaped-quotes, a.attribute-value));
end method transform;

define method transform(str :: <char-string>, state :: <printing>)
  print-object(str, state.stream);
end method transform;

define method escape-xml (symbol :: <symbol>) => (res :: <symbol>)
  as(<symbol>, escape-xml(as(<string>, symbol)))
end method escape-xml;

define method escape-xml (string :: <string>) => (res :: <string>)
  let res = "";
  for (char in string)
    if (char = '>')
      res := concatenate(res, "&gt;");
    elseif (char = '<')
      res := concatenate(res, "&lt;");
    elseif (char = '&')
      res := concatenate(res, "&amp;");
    elseif (char = '\'')
      res := concatenate(res, "&apos;");      
    elseif (char = '"')
      res := concatenate(res, "&quot;");      
    else
      res := add!(res, char)
    end;
  end;
  res;
end method escape-xml;

define method print-object(str :: <char-string>, s :: <stream>) => ()
  print-filled-string(escape-xml(str.text), s);
end method print-object;

define function print-escaped-quotes(string :: <string>, stream :: <stream>) => ()
  write-element(stream, '"');
  write(stream, escape-xml(string));
  write-element(stream, '"');
end;

define function print-filled-string(str :: <string>, s :: <stream>) => ()
  let last-was-whitespace? = #f;
  let looking-at-whitespace? = #f;
  for(ch in str)
    looking-at-whitespace? := #f;
    select(ch)
      ' ', '\r', '\n' => begin
                           unless (last-was-whitespace?)
                             write-element(s, ' ');
                             pprint-newline(#"fill", s);
                           end;
                           looking-at-whitespace? := #t;
                         end;
      otherwise => write-element(s, ch);
    end select;
    if (looking-at-whitespace?)
      last-was-whitespace? := #t;
    else
      last-was-whitespace? := #f;
    end;
  end for;
end function print-filled-string;

define method transform(ref :: <reference>, state :: <printing>)
  write(state.stream, ref.xml-name);
end method transform;

define method print-object(ref :: <reference>, s :: <stream>) => ()
  let state = make(<printing>, stream: s);
  transform(ref, state);
end method print-object;

define method print-object(doc :: <document>, s :: <stream>) => ()
  let state = make(<printing>, stream: s);
  transform(doc, state);
end method print-object;

define method as (class == <string>, xml :: <xml>)
 => (res :: <string>);
  let stream = make(<string-stream>, direction: #"output");
  print-object(xml, stream);
  stream-contents(stream);
end method as;

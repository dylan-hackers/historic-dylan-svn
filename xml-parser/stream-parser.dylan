Module: xml-stream-parser

/*
** - monitor
** - parse -> dispatch -> handle
*/

define class <xml-stream-parser> (<object>)
  slot stream :: <stream>,
    required-init-keyword: stream:;

  // Maps the symbols #"start-element", #"end-element", and #"characters" to
  // SAX-style event handler functions.  The #"start-element" handler receives
  // the element name and a sequence (why not a table?) of attributes.
  // The #"end-element" handler receives just the element name.  The #"characters"
  // handler receives a string.
  constant slot handlers :: <table> = make(<table>);

  constant slot opened-elements :: <deque> = make(<deque>);
  slot text-buffer :: <string> = "";
  slot tag-buffer :: <string> = "";
  slot parsing-tag? :: <boolean> = #f;
  slot parsing-root? :: <boolean> = #f;
end class <xml-stream-parser>;

define method stream-parse-error
    (parser :: <xml-stream-parser>, format-string :: <string>,
     #rest format-arguments)
  let position-string = format-to-string("XML parse error at position %d: ",
                                         stream-position(parser.stream));
  signal(make(<xml-parse-error>,
              format-string: concatenate(position-string, format-string),
              format-arguments: format-arguments))
end method stream-parse-error;

// Register a handler for an event, a la SAX parsers.
//
define method monitor (parser :: <xml-stream-parser>,
                       event :: one-of(#"start-element", #"end-element", #"characters"),
                       handler :: <function>)
  parser.handlers[event] := handler;
end method monitor;

define method parse (parser :: <xml-stream-parser>)
  while (~ stream-at-end?(parser.stream))
    let received = read-element(parser.stream);
    dispatch(parser, received, parser.parsing-tag?, parser.parsing-root?);
  end while;
  if (~empty?(parser.opened-elements))
    stream-parse-error(parser, "Reached end of stream while parsing %s element",
                       pop-last(parser.opened-elements));
  end;
end method parse;

define generic dispatch
    (parser :: <xml-stream-parser>,
     char :: <character>,
     in-tag? :: <boolean>,
     in-root? :: <boolean>)
 => ();

// This is generally the method that gets called for the first character
// in the stream.  e.g., if the stream starts with "<?xml..."
//
define method dispatch (parser :: <xml-stream-parser>,
                        char == '<', in-tag? == #f, in-root? == #f)
 => ()
  parser.parsing-tag? := #t;
  parser.tag-buffer := add!(parser.tag-buffer, char);
end method;

define method dispatch (parser :: <xml-stream-parser>,
                        char == '<', in-tag? == #f, in-root? == #t)
 => ()
  handle-characters(parser, parser.text-buffer);
  parser.text-buffer := "";
  parser.parsing-tag? := #t;
  parser.tag-buffer := add!(parser.tag-buffer, char);
end method;

define method dispatch (parser :: <xml-stream-parser>,
                        char :: <character>, in-tag? == #f, in-root? == #f)
 => ()
  if (~ whitespace?(char))
    stream-parse-error(parser,
                       "Non-whitespace character data found outside root element.");
  end if;
end method;

define method dispatch (parser :: <xml-stream-parser>,
                        char :: <character>, in-tag? == #f, in-root? == #t)
 => ()
  parser.text-buffer := add!(parser.text-buffer, char);
end method;
 
define method dispatch (parser :: <xml-stream-parser>,
                        char :: <character>, in-tag? == #t, in-root? :: <boolean>)
 => ()
  if (~ whitespace?(char))
    parser.tag-buffer := add!(parser.tag-buffer, char);
  end if;
end method;

define method dispatch (parser :: <xml-stream-parser>,
                        char == '>', in-tag? == #t, in-root? :: <boolean>)
 => ()
  next-method();

  block (skip-next)
    let (index, start-tag, attributes, opened-element?)
      = scan-start-tag(parser.tag-buffer);
    if (start-tag)
      start-tag := as(<string>, start-tag);
      handle-start-element(parser, start-tag, attributes);
      if (opened-element?) 
        push-last(parser.opened-elements, start-tag) 
      else
        handle-end-element(parser, start-tag);
      end if;
      parser.parsing-root? := #t;
      skip-next();
    end if;
    
    let (index, end-tag) = scan-end-tag(parser.tag-buffer);
    if (end-tag)
      let begin-tag = pop-last(parser.opened-elements);
      if (begin-tag = end-tag)
        handle-end-element(parser, end-tag);
      else
        stream-parse-error(parser, "Tag mismatch: <%s> ... </%s>",
                           begin-tag, end-tag);
      end if;
      if (size(parser.opened-elements) = 0)
        in-root? := #f;
      end if;
      skip-next();
    end if;

    let (index, processing-instruction) = scan-xml-decl(parser.tag-buffer);
    if (processing-instruction)
//!!! call handle-processing-instruction (processing-instruction)
      skip-next();
    end if;
  end block;
  parser.tag-buffer := "";
  parser.parsing-tag? := #f;
end method;

define method whitespace? (char :: <character>) 
 => (res :: <boolean>);
  instance?(char, one-of('\n', '\t', '\r'))
end method whitespace?;

define method handle-start-element
    (parser :: <xml-stream-parser>, name, attributes :: <sequence>)
  let receiver = element(parser.handlers, #"start-element", default: #f);
  if (receiver)
    receiver(name, attributes);
  end;
end method handle-start-element;

define method handle-end-element (parser :: <xml-stream-parser>, name)
  let receiver = element(parser.handlers, #"start-element", default: #f);
  if (receiver)
    receiver(name);
  end;
end method handle-end-element;

define method handle-characters (parser :: <xml-stream-parser>, text :: <string>)
  let receiver = element(parser.handlers, #"start-element", default: #f);
  if (receiver)
    receiver(text);
  end;
end method handle-characters;

module: xml-stream-parser
synopsis: 
author: 
copyright:

/*
** - monitor
** - parse -> dispatch -> handle
*/

define class <xml-stream-parser> (<object>)
  slot stream :: <stream>,
    required-init-keyword: stream:;
  slot handlers :: <table> = make(<table>);
  slot opened-elements :: <deque> = make(<deque>);
  slot text-buffer :: <string> = "";
  slot tag-buffer :: <string> = "";
  slot parsing-tag? :: <boolean> = #f;
  slot parsing-root? :: <boolean> = #f;
end class <xml-stream-parser>;

define method monitor (parser :: <xml-stream-parser>, event :: one-of(#"start-element", #"end-element", #"characters"), handler :: <function>)
  parser.handlers[event] := handler;
end method monitor;
  
define method parse (parser :: <xml-stream-parser>)
  while (~ stream-at-end?(parser.stream))
    let received = read-element(parser.stream);
    dispatch(parser, received, parser.parsing-tag?, parser.parsing-root?);
  end while;
end method parse;

define generic dispatch (parser :: <xml-stream-parser>, char :: <character>, in-tag? :: <boolean>, in-root? :: <boolean>) => ();

define method dispatch (parser :: <xml-stream-parser>, char == '<', in-tag? == #f, in-root? :: <boolean>) => ();
  parser.parsing-tag? := #t;
  parser.tag-buffer := add!(parser.tag-buffer, char);
end method;

define method dispatch (parser :: <xml-stream-parser>, char == '<', in-tag? == #f, in-root? == #t) => ();
  handle-characters(parser, parser.text-buffer);
  parser.text-buffer := "";
  parser.parsing-tag? := #t;
  parser.tag-buffer := add!(parser.tag-buffer, char);
end method;

define method dispatch (parser :: <xml-stream-parser>, char :: <character>, in-tag? == #f, in-root? == #f) => ();
  if (~ whitespace?(char))
//!!! error (chars outside root element)
  end if;
end method;

define method dispatch (parser :: <xml-stream-parser>, char :: <character>, in-tag? == #f, in-root? == #t) => ();
  parser.text-buffer := add!(parser.text-buffer, char);
end method;
 
define method dispatch (parser :: <xml-stream-parser>, char :: <character>, in-tag? == #t, in-root? :: <boolean>) => ();
  if (~ whitespace?(char))
    parser.tag-buffer := add!(parser.tag-buffer, char);
  end if;
end method;

define method dispatch (parser :: <xml-stream-parser>, char == '>', in-tag? == #t, in-root? :: <boolean>) => ();
  next-method();

  block (skip-next)
    let (index, start-tag, attributes, opened-element?) = scan-start-tag(parser.tag-buffer);
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
      if (end-tag ~= last(parser.opened-elements))
//!!! error (tag mismatch)
      else
        pop-last(parser.opened-elements);
        handle-end-element(parser, end-tag);
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

define method handle-start-element (parser :: <xml-stream-parser>, name, attributes :: <sequence>)
  parser.handlers[#"start-element"](name, attributes);
end method handle-start-element;

define method handle-end-element (parser :: <xml-stream-parser>, name)
  parser.handlers[#"end-element"](name);
end method handle-end-element;

define method handle-characters (parser :: <xml-stream-parser>, text :: <string>)
  parser.handlers[#"characters"](text);
end method handle-characters;

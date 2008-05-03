module: markup-parser

// exported
define class <ascii-line-token> (<token>, <token-source>)
   slot content :: <string>, init-keyword: #"content"
end;


define parser-method eos (stream, context)
=> (token :: <symbol>)
   let pos = stream.stream-position;
   if (~stream.stream-at-end?)
      error(make(<parse-failure>, position: pos, expected: "end of stream"))
   end if;
   #"eos"
end parser-method;


define method count-spaces (stream, context, #key to :: false-or(<integer>) = #f)
=> (count :: <integer>)
   for (count from 0,
        while: peek(stream, on-end-of-stream: #f) = ' '
               & if (to) count < to else #t end)
      read-element(stream)
   finally count
   end for
end method;


define parser-method opt-spaces (stream, context)
=> (token :: false-or(<symbol>))
   let count = count-spaces(stream, context);
   (count ~= 0) & (#"opt-spaces")
end parser-method;


define parser-method spaces (stream, context)
=> (token :: <symbol>)
   let count = count-spaces(stream, context);
   if (count = 0)
      error(make(<parse-failure>, position: stream.stream-position, expected: "spaces"))
   end;
   #"spaces"
end parser-method;


define parser-method sol (stream, context)
=> (false :: <boolean>)
   // This method *may* skip to the start of the next line of a comment block
   // or check an EOL status in the context, or else is a no-op.
   if (stream.stream-at-end?)
      error(make(<parse-failure>, position: stream.stream-position,
                 expected: "start of line"))
   end;
   #f
end parser-method;


define parser-method sol-ind (stream, context)
=> (token :: <symbol>)
   let pos = stream.stream-position;
   parse-sol(stream, context);
   let expected-level =
         if (context.indent-stack.empty?) 0 else context.indent-stack.first end;
   let actual-level = count-spaces(stream, context, to: expected-level);
   if (actual-level < expected-level)
      error(make(<parse-failure>, position: pos, expected:
               format-to-string("flush line at %d (got %d)", expected-level, actual-level)));
   end;
   #"sol-ind"
end parser-method;


define parser-method opt-new-sol-ind (stream, context)
=> (token :: false-or(<symbol>))
   parse-sol(stream, context);
   let pos = stream.stream-position;
   let indent-level =
         if (context.indent-stack.empty?) 0 else context.indent-stack.first end;
   let spaces = count-spaces(stream, context);
   stream.stream-position := pos;
   case
      spaces > indent-level =>
         push(context.indent-stack, spaces);
         #"opt-new-sol-ind";
      spaces = indent-level =>
         #f;
      spaces < indent-level =>
         error(make(<parse-failure>, position: pos,
                    expected: "flush or indented line"));
   end case
end;


define parser-method new-sol-ind (stream, context)
=> (token :: <symbol>)
   parse-sol(stream, context);
   let pos = stream.stream-position;
   let indent-level =
         if (context.indent-stack.empty?) 0 else context.indent-stack.first end;
   let spaces = count-spaces(stream, context);
   stream.stream-position := pos;
   case
      spaces > indent-level =>
         push(context.indent-stack, spaces);
         #"new-sol-ind";
      otherwise =>
         error(make(<parse-failure>, position: pos,
                    expected: "indented line"));
   end case
end parser-method;


define parser-method ls (stream, context)
=> (token :: <symbol>)
   let pos = stream.stream-position;
   let char = read-element(stream, on-end-of-stream: #f);
   select (char)
      '\<0d>' =>
         if (peek(stream, on-end-of-stream: #f) = '\<0a>')
            read-element(stream)
         end if;
      '\<0a>', '\<0c>' /* (no Unicode) '\<2028>', '\<2029>' */ =>
         ;
      otherwise =>
         error(make(<parse-failure>, position: pos, expected: "new line"));
   end select;
   #"ls"
end parser-method;


define parser-method char (stream, context)
=> (character :: <character>)
   let pos = stream.stream-position;
   let char = read-element(stream, on-end-of-stream: #f);
   if (~char)
      error(make(<parse-failure>, position: pos, expected: "character"))
   end if;
   char
end parser-method;


define parser-method number (stream, context)
=> (token :: <integer>)
   let pos = stream.stream-position;
   let num-str = "";
   while (~stream.stream-at-end? & digit?(peek(stream)))
      num-str := add!(num-str, read-element(stream))
   end while;
   if (num-str.empty?)
      error(make(<parse-failure>, position: pos, expected: "number"))
   end if;
   string-to-integer(num-str)
end parser-method;


define parser-method ordinal (stream, context)
=> (token :: <character>)
   let pos = stream.stream-position;
   let char = peek(stream, on-end-of-stream: #f);
   if (~char | ~alphabetic?(char))
      error(make(<parse-failure>, position: pos, expected: "ordinal character"))
   end if;
   read-element(stream);
   char
end parser-method;


define parser-method spc-or-ls (stream, context)
=> (token :: <symbol>)
   let pos = stream.stream-position;
   let char = read-element(stream, on-end-of-stream: #f);
   select (char)
      ' ', '\<0d>', '\<0a>', '\<0c>' /* (no Unicode) '\<2028>', '\<2029>' */ =>
         #"spc-or-ls";
      otherwise =>
         error(make(<parse-failure>, position: pos, expected: "space or new line"));
   end select
end parser-method;


define method count-spc-or-ls (stream, context)
=> (count :: <integer>)
   for (count from 0,
        while: member?(peek(stream, on-end-of-stream: #f),
                       " \<0d>\<0a>\<0c>" /* (no Unicode) "\<2028>\<2029>" */ ))
      read-element(stream)
   finally
      count
   end for
end method;


define parser-method many-spc-or-ls (stream, context)
=> (token :: <symbol>)
   let pos = stream.stream-position;
   let count = count-spc-or-ls(stream, context);
   if (count = 0)
      error(make(<parse-failure>, position: pos, expected: "spaces or new line"))
   end if;
   #"many-spc-or-ls"
end parser-method;


define parser-method opt-many-spc-or-ls (stream, context)
=> (token :: false-or(<symbol>))
   let count = count-spc-or-ls(stream, context);
   (count ~= 0) & (#"many-spc-or-ls")
end parser-method;


define parser-method flush-content-at-level (stream, context)
=> (content :: <sequence>)
   let ind? = parse-opt-new-sol-ind(stream, context);
   block()
      parse-lines-content(stream, context);
   cleanup
      if (ind?) pop(context.indent-stack) end;
   end block
end parser-method;


define parser-method indented-content (stream, context)
=> (content :: <sequence>)
   parse-new-sol-ind(stream, context);
   block()
      parse-flush-content(stream, context);
   cleanup
      pop(context.indent-stack);
   end block;
end parser-method;


define parser-method indented-link-words (stream, context)
=> (content :: <sequence>)
   parse-new-sol-ind(stream, context);
   block()
      parse-link-word-lines(stream, context);
   cleanup
      pop(context.indent-stack);
   end block;
end parser-method;


define parser-method text-til-ls (stream, context)
=> (text :: <string>)
   let pos = stream.stream-position;
   let text = "";
   while (~member?(peek(stream, on-end-of-stream: '\<00>'),
                   "\<00>\<0d>\<0a>\<0c>" /* (no Unicode) "\<2028>\<2029>" */))
      text := add!(text, read-element(stream))
   end while;
   if (text.empty?)
      error(make(<parse-failure>, position: pos, expected: "text"))
   end if;
   text
end parser-method;


define parser-method text-til-spc-or-ls (stream, context)
=> (text :: <string>)
   let pos = stream.stream-position;
   let text = "";
   while (~member?(peek(stream, on-end-of-stream: '\<00>'),
                   " \<00>\<0d>\<0a>\<0c>" /* (no Unicode) "\<2028>\<2029>" */))
      text := add!(text, read-element(stream))
   end while;
   if (text.empty?)
      error(make(<parse-failure>, position: pos, expected: "text"))
   end if;
   text
end parser-method;


define parser-method text-til-end-quote (stream, context)
=> (text :: <string>)
   let pos = stream.stream-position;
   let (text, found?) = read-to(stream, context.end-quote-char, on-end-of-stream: #f);
   if ((text & text.size = 0) | ~found?)
      error(make(<parse-failure>, position: pos, expected: "text"))
   end if;
   text
end parser-method;


define parser-method start-quote (stream, context)
=> (quote :: <string>)
   let pos = stream.stream-position;
   let quote = read-element(stream, on-end-of-stream: #f);
   case
      member?(quote, $open-quote-chars) =>
         let quote-index = position($open-quote-chars, quote);
         context.end-quote-char := $close-quote-chars[quote-index];
      otherwise =>
         if (quote) unread-element(stream, quote) end;
         error(make(<parse-failure>, position: stream.stream-position,
                    expected: concatenate("one of \"", $open-quote-chars, "\"")));
   end case;
   as(<string>, quote);
end;


define parser-method end-quote (stream, context)
=> (quote :: <string>)
   let pos = stream.stream-position;
   let quote = read-element(stream, on-end-of-stream: #f);
   select (quote)
      context.end-quote-char =>
         context.end-quote-char := #f;
         as(<string>, quote);
      otherwise =>
         if (quote) unread-element(stream, quote) end;
         let expected-str = format-to-string("\"%c\"", context.end-quote-char);
         error(make(<parse-failure>, position: pos, expected: expected-str));
   end select
end;


define parser-method bullet-char (stream, context)
=> (bullet :: <string>)
   let pos = stream.stream-position;
   let char = peek(stream, on-end-of-stream: #f);
   if (~member?(char, $bullet-chars))
      let exp-str = concatenate("one of \"", $bullet-chars, "\"");
      error(make(<parse-failure>, position: pos, expected: exp-str));
   end if;
   read-element(stream);
   as(<string>, char);
end;


define parser-method ascii-line (stream, context)
=> (token :: <ascii-line-token>)
   let pos = stream.stream-position;
   let ascii-line = "";
   let ascii-char = peek(stream, on-end-of-stream: #f);
   case
      context.ascii-line-char & context.ascii-line-char ~= ascii-char =>
         error(make(<parse-failure>, position: pos,
                    expected: format-to-string("\"%c\"", context.end-quote-char)));
      member?(ascii-char, $ascii-line-chars) =>
         ascii-line := read-to(stream, ascii-char, test: \~=, on-end-of-stream: #f);
   end case;
   when (ascii-line.size < 3)
      error(make(<parse-failure>, position: pos,
                 expected: concatenate("several of \"", $ascii-line-chars, "\"")))
   end when;
   unless (context.ascii-line-char)
      context.ascii-line-char := ascii-char;
   end unless;
   make(<ascii-line-token>, start: pos, end: stream.stream-position, content: ascii-line)
end parser-method;


define parser-method table-header (stream, context) => (token)
   error(make(<parse-failure>, position: stream.stream-position,
              expected: "table header"));
end;

define parser-method table-row (stream, context) => (token)
   error(make(<parse-failure>, position: stream.stream-position,
              expected: "table row"));
end;

define parser-method table-footer (stream, context) => (token)
   error(make(<parse-failure>, position: stream.stream-position,
              expected: "table footer"));
end;

define parser-method api-ref (stream, context) => (token)
   error(make(<parse-failure>, position: stream.stream-position,
              expected: "api reference"));
end;

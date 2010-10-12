module: markup-parser


define constant $indent = '\<11>';
define constant $dedent = '\<12>';


define parser-method indent (stream, context) => (token :: false-or(<character>))
   label "indent";
   let ind = read-element(stream, on-end-of-stream: #f);
   when (ind = $indent)
      $indent
   end when;
end parser-method;


define parser-method dedent (stream, context) => (token :: false-or(<character>))
   label "dedent";
   let ded = read-element(stream, on-end-of-stream: #f);
   when (ded = $dedent)
      $dedent
   end when;
end parser-method;


define parser-method char (stream, context)
=> (character :: false-or(<character>))
   label "character";
   let elem = read-element(stream, on-end-of-stream: #f);
   ~(elem = $indent | elem = $dedent) & elem;
end parser-method;


define parser-method number (stream, context)
=> (integer :: false-or(<integer>))
   label "number";
   let num-str = "";
   iterate cont ()
      let next = peek(stream, on-end-of-stream: #f);
      if (instance?(next, <character>) & next.digit?)
         num-str := add!(num-str, read-element(stream));
         cont();
      end if;
   end iterate;
   // String-to-integer actually returns two values; we only want the first one.
   let res = ~num-str.empty? & string-to-integer(num-str);
   res;
end parser-method;


define parser-method ordinal (stream, context)
=> (ordinal :: false-or(<character>))
   label "ordinal character (a-z)";
   let elem = peek(stream, on-end-of-stream: #f);
   elem & alphabetic?(elem) & read-element(stream);
end parser-method;


define method parse-literal
   (stream :: <positionable-stream>, string :: <string>, context)
=> (token :: false-or(<symbol>), succ? :: <boolean>, err :: false-or(<parse-failure>))
   let string = as-lowercase(string);
   let pos = stream.stream-position;
   let stream-elems = read(stream, string.size, on-end-of-stream: #f);
   let stream-string = stream-elems & as(<string>, stream-elems);
   if (~stream-string | as-lowercase(stream-string) ~= string)
      let expected = format-to-string("%=", string);
      values(#f, #f, make(<parse-failure>, position: pos, expected: expected));
   else
      values(as(<symbol>, string), #t, #f)
   end if;
end method;

define method parse-literal
   (stream :: <positionable-stream>, char :: <character>, context)
=> (token :: false-or(<symbol>), succ? :: <boolean>, err :: false-or(<parse-failure>))
   let pos = stream.stream-position;
   let stream-char = read-element(stream, on-end-of-stream: #f);
   if (stream-char ~= char)
      let expected = format-to-string("%=", as(<string>, char));
      values(#f, #f, make(<parse-failure>, position: pos, expected: expected));
   else
      values(as(<symbol>, as(<string>, stream-char)), #t, #f);
   end if;
end method;

define macro literal-parsers-definer
   { define literal-parsers ?literals end } => { ?literals }
literals:
   { ?:name = ?:expression; ... }
      => {  define parser-method ?name (stream, context)
            => (token :: false-or(<symbol>), succ? :: <boolean>,
                err :: false-or(<parse-failure>))
               label format-to-string("\"%s\"", as(<string>, ?expression));
               parse-literal(stream, ?expression, context)
            end; 
            ... }
   { } => { }
end macro;

define literal-parsers
   ls             = '\n';
   spc            = ' ';
   colon          = ':';
   percent        = '%';
   lt             = '<';
   gt             = '>';
   bar            = '|';
   left-brace     = '{';
   left-paren     = '(';
   right-paren    = ')';
   hash           = '#';
   hyphen         = '-';
   open-bracket   = '[';
   close-bracket  = ']';
   period         = '.';
   also-lit       = "also";
   api-lit        = "api";
   args-lit       = "args";
   arguments-lit  = "arguments";
   b-lit          = "b";
   bib-lit        = "bib";
   bindings-lit   = "bindings";
   class-lit      = "class";
   classes-lit    = "classes";
   code-lit       = "code";
   conditions-lit = "conditions";
   constant-lit   = "constant";
   diagram-lit    = "diagram";
   discussion-lit = "discussion";
   dita-lit       = "dita";
   ditto-lit      = "ditto";
   em-lit         = "em";
   end-lit        = "end";
   errors-lit     = "errors";
   example-lit    = "example";
   exceptions-lit = "exceptions";
   fig-lit        = "fig";
   fully-lit      = "fully";
   function-lit   = "function";
   functions-lit  = "functions";
   generic-lit    = "generic";
   html-lit       = "html";
   i-lit          = "i";
   img-lit        = "img";
   in-lit         = "in";
   init-keywords-lit = "init-keywords";
   keywords-lit   = "keywords";
   libraries-lit  = "libraries";
   library-lit    = "library";
   list-lit       = "list";
   macro-lit      = "macro";
   macros-lit     = "macros";
   make-lit       = "make";
   method-lit     = "method";
   module-lit     = "module";
   modules-lit    = "modules";
   name-lit       = "name";
   names-lit      = "names";
   note-lit       = "note";
   of-lit         = "of";
   parent-lit     = "parent";
   q-lit          = "q";
   qq-lit         = "qq";
   qualified-lit  = "qualified";
   qv-lit         = "qv";
   relevant-lit   = "relevant";
   section-lit    = "section";
   see-lit        = "see";
   sic-lit        = "sic";
   signals-lit    = "signals";
   syn-lit        = "syn";
   synopsis-lit   = "synopsis";
   term-lit       = "term";
   to-lit         = "to";
   topic-lit      = "topic";
   u-lit          = "u";
   unbound-lit    = "unbound";
   unq-lit        = "unq";
   values-lit     = "values";
   variable-lit   = "variable";
   variables-lit  = "variables";
   verbatim-lit   = "verbatim";
   vi-lit         = "vi";
   warning-lit    = "warning";
   x-lit          = "x";
end;

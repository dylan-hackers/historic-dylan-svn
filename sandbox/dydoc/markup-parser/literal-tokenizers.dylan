module: markup-parser

define method parse-literal
   (stream :: <positionable-stream>, string :: <string>, context)
=> (token :: <symbol>)
   let string = as-lowercase(string);
   let pos = stream.stream-position;
   let stream-string = read(stream, string.size, on-end-of-stream: #f);
   if (~stream-string | as-lowercase(stream-string) ~= string)
      error(make(<parse-failure>, position: pos,
                 expected: concatenate("\"", string, "\"")))
   end if;
   as(<symbol>, string)
end method;

define method parse-literal
   (stream :: <positionable-stream>, char :: <character>, context)
=> (token :: <symbol>)
   let pos = stream.stream-position;
   let stream-char = read-element(stream, on-end-of-stream: #f);
   if (stream-char ~= char)
      let expected-str = format-to-string("\"%c\"", char);
      error(make(<parse-failure>, position: pos,
                 expected: expected-str))
   end if;
   as(<symbol>, as(<string>, stream-char))
end method;

define macro literal-parsers-definer
   { define literal-parsers ?literals end } => { ?literals }
literals:
   { ?:name = ?:expression; ... }
      => {  define parser-method ?name (stream, context) => (token)
               parse-literal(stream, ?expression, context)
            end; 
            ... }
   { } => { }
end macro;

define literal-parsers
   colon          = ':';
   percent        = '%';
   spc            = ' ';
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
   args-lit       = "args";
   arguments-lit  = "arguments";
   b-lit          = "b";
   bib-lit        = "bib";
   bindings-lit   = "bindings";
   class-lit      = "class";
   classes-lit    = "classes";
   code-lit       = "code";
   conditions-lit = "conditions";
   contents-lit   = "contents";
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
   function-lit   = "function";
   functions-lit  = "functions";
   generic-lit    = "generic";
   html-lit       = "html";
   i-lit          = "i";
   img-lit        = "img";
   init-keywords-lit = "init-keywords";
   keywords-lit   = "keywords";
   libraries-lit  = "libraries";
   library-lit    = "library";
   list-lit       = "list";
   macro-lit      = "macro";
   macros-lit     = "macros";
   make-lit       = "make";
   module-lit     = "module";
   modules-lit    = "modules";
   note-lit       = "note";
   of-lit         = "of";
   q-lit          = "q";
   qq-lit         = "qq";
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
   toc-lit        = "toc";
   u-lit          = "u";
   unq-lit        = "unq";
   values-lit     = "values";
   variable-lit   = "variable";
   variables-lit  = "variables";
   verbatim-lit   = "verbatim";
   warning-lit    = "warning";
   x-lit          = "x";
end;

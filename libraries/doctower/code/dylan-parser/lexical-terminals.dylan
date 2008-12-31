module: dylan-parser
synopsis: Dylan grammar terminals, customized to allow documentation comments.

define constant $eof = make(<parse-failure>, expected-other-than: "end of file");


define method read-matching-string (stream, string :: <string>)
=> (str :: false-or(<string>))
   let in-str = read(stream, string.size, on-end-of-stream: #f);
   case
      in-str & case-insensitive-equal?(in-str, string) => in-str;
      otherwise => #f;
   end case;
end method;

define method read-one-of-char (stream, string :: <string>)
=> (str :: false-or(<string>))
   let in-char = read-element(stream, on-end-of-stream: #f);
   case
      in-char & member?(as-lowercase(in-char), string) => as(<string>, in-char);
      otherwise => #f;
   end case;
end method;


define macro lexical-parsers-definer
   { define lexical-parsers ?parse-items end } => { ?parse-items }
parse-items:
   { ?:name = ?:expression ; ... } => {
      define parser-method ?name (stream, context)
      => (string :: false-or(<string>))
         label format-to-string("%=", ?expression);
         read-matching-string(stream, ?expression);
      end parser-method;
      ...
   }
   { ?:name in ?:expression label ?label:expression ; ... } => {
      define parser-method ?name (stream, context)
      => (char :: false-or(<string>))
         label ?label;
         read-one-of-char(stream, ?expression);
      end parser-method;
      ...
   }
   { } => { }
end macro;

define lexical-parsers
   lit-begin         =  "begin";
   lit-block         =  "block";
   lit-case          =  "case";
   lit-class         =  "class";
   lit-constant      =  "constant";
   lit-define        =  "define";
   lit-domain        =  "domain";
   lit-end           =  "end";
   lit-for           =  "for";
   lit-function      =  "function";
   lit-generic       =  "generic";
   lit-handler       =  "handler";
   lit-if            =  "if";
   lit-let           =  "let";
   lit-library       =  "library";
   lit-local         =  "local";
   lit-macro         =  "macro";
   lit-method        =  "method";
   lit-module        =  "module";
   lit-otherwise     =  "otherwise";
   lit-select        =  "select";
   lit-unless        =  "unless";
   lit-until         =  "until";
   lit-variable      =  "variable";
   lit-while         =  "while";

   lit-all           =  "all";
   lit-create        =  "create";
   lit-exclude       =  "exclude";
   lit-export        =  "export";
   lit-import        =  "import";
   lit-inherited     =  "inherited";
   lit-init-function =  "init-function";
   lit-init-keyword  =  "init-keyword";
   lit-init-value    =  "init-value";
   lit-keyword       =  "keyword";
   lit-prefix        =  "prefix";
   lit-rename        =  "rename";
   lit-req-init-keyword = "required-init-keyword";
   lit-required      =  "required";
   lit-sealed        =  "sealed";
   lit-setter        =  "setter";
   lit-slot          =  "slot";
   lit-type          =  "type";
   lit-use           =  "use";
                         
   colon             =  ":";
   double-colon      =  "::";
   arrow             =  "=>";
   double-question   =  "??";
   question-equal    =  "?=";
   question          =  "?";
   ellipsis          =  "...";
   double-pound      =  "##";
   pound             =  "#";
   double-slash      =  "//";
   triple-slash      =  "///";
   lf-comment        =  "/*";
   lf-doc-comment    =  "/**";
   rt-comment        =  "*/";
                         
   empty-list        =  "#()";
   true              =  "#t";
   false             =  "#f";
   bin               =  "#b";
   oct               =  "#o";
   hex               =  "#x";
   next              =  "#next";
   rest              =  "#rest";
   key               =  "#key";
   all-keys          =  "#all-keys";
   include           =  "#include";
   lf-list           =  "#(";
   lf-vector         =  "#[";
                     
   identical         =  "==";
   not-identical     =  "~==";
   not-equal         =  "~=";
   lt-equal          =  "<=";
   gt-equal          =  ">=";
   bind              =  ":=";
   plus              =  "+";
   minus             =  "-";
   star              =  "*";
   slash             =  "/";
   caret             =  "^";
   equal             =  "=";
   lt                =  "<";
   gt                =  ">";
   amp               =  "&";
   vert-bar          =  "|";
   not               =  "~";
                     
   apos              =  "'";
   backslash         =  "\\";
   quote             =  "\"";
   period            =  ".";
   exp               =  "e";
   comma             =  ",";
   semicolon         =  ";";
   esc-char          in "\\\'\"abefnrt0"  label "escape character";
                     
   lf-paren          =  "(";
   rt-paren          =  ")";
   lf-brack          =  "[";
   rt-brack          =  "]";
   lf-brace          =  "{";
   rt-brace          =  "}";
end;


define parser-method char (stream, context)
=> (char :: false-or(<string>),
    succ? :: <boolean>, err :: false-or(<parse-failure>))
   label "character";
   let char = read-element(stream, on-end-of-stream: #f);
   case
      char =>        values(as(<string>, char), #t, #f);
      otherwise =>   values(#f, #f, $eof);
   end case;
end parser-method;

define parser-method printing-character (stream, context)
=> (char :: false-or(<string>), succ? :: <boolean>,
    err :: false-or(<parse-failure>))
   label "printable character";
   let char = read-element(stream, on-end-of-stream: #f);
   case
      char & char.printable? =>  values(as(<string>, char), #t, #f);
      ~char =>                   values(#f, #f, $eof);
      otherwise =>               values(#f, #f, #f);
   end case;
end parser-method;

define parser-method eol (stream, context)
=> (eol :: false-or(singleton(#"eol")),
    succ? :: <boolean>, err :: false-or(<parse-failure>))
   label "end of line";
   let char = read-element(stream, on-end-of-stream: #f);
   select (char)
      '\n' => values(#"eol", #t, #f);
      #f   => values(#f, #f, $eof);
      otherwise => values(#f, #f, #f); 
   end select;
end parser-method;

define parser-method spc (stream, context)
=> (spc :: false-or(singleton(#"spc")),
    succ? :: <boolean>, err :: false-or(<parse-failure>))
   label "space";
   let char = read-element(stream, on-end-of-stream: #f);
   select (char)
      ' ' => values(#"spc", #t, #f);
      #f  => values(#f, #f, $eof);
      otherwise => values(#f, #f, #f); 
   end select;
end parser-method;

define parser-method line (stream, context)
=> (spc :: false-or(singleton(#"line")),
    succ? :: <boolean>, err :: false-or(<parse-failure>))
   label "line";
   let line = read-line(stream, on-end-of-stream: #f);
   select (line)
      #f => values(#f, #f, $eof);
      otherwise => values(#"line", #t, #f);
   end select;
end parser-method;

module: template-engine


//
// Base lexeme tokens
//


define class <lexeme> (<token>)
   slot text :: <string>
end class;

define class <value> (<lexeme>, <expression-token>)
end class;


//
// Convenience macros
//


define macro lexemes-definer
   { define lexemes ?parse-items end } => { ?parse-items }
parse-items:
   { ?:name = ?:expression ?sep:name; ... }
   => {  define literals
            ?name = ?expression
         end literals;
      
         define parser "lex-" ## ?name (<lexeme>)
            rule seq("lit-" ## ?name, ?sep) => tokens;
            inherited slot text = tokens[0];
         end parser;
         
         ... }
   { } => { }
end macro;

define macro literals-definer
   { define literals ?parse-items end } => { ?parse-items }
parse-items:
   { ?:name = ?:expression; ... }
   => {  define parser-method "lit-" ## ?name (stream, context)
         => (string :: false-or(<string>))
            label format-to-string("\"%s\"", ?expression);
            read-matching-string(stream, ?expression);
         end parser-method;         
         ... }
   { } => { }
end macro;


//
// Lexemes
//


define lexemes
   \EXCLAMATION = "!"         non-name-sep;
   \PERIOD      = "."         nil-sep;
   \COMMA       = ","         nil-sep;
   \NOT         = "~"         not-sep;
   \AMPERSAND   = "&"         non-name-sep;
   \VERT-BAR    = "|"         non-name-sep;
   \PLUS        = "+"         nil-sep;
   \MINUS       = "-"         nil-sep;
   \STAR        = "*"         star-sep;
   \SLASH       = "/"         nil-sep;
   \PERCENT     = "%"         non-name-sep;
   \EQUAL       = "="         non-name-sep;
   \NOT-EQUAL   = "~="        nil-sep;
   \LTE         = "<="        nil-sep;
   \GTE         = ">="        nil-sep;
   \LF-ANGLE    = "<"         lf-angle-sep;
   \RT-ANGLE    = ">"         rt-angle-sep;
   \LF-PAREN    = "("         lf-paren-sep;
   \RT-PAREN    = ")"         rt-paren-sep;  
   \LF-BRACK    = "["         lf-brack-sep;
   \RT-BRACK    = "]"         rt-brack-sep;  
   \LF-BRACE    = "{"         lf-brace-sep;
   \RT-BRACE    = "}"         rt-brace-sep;
   \LF-PARENS   = "(("        nil-sep;
   \RT-PARENS   = "))"        nil-sep;
   \LF-ANGLES   = "<<"        nil-sep;
   \RT-ANGLES   = ">>"        nil-sep;
   \LF-BRACKS   = "[["        nil-sep;
   \RT-BRACKS   = "]]"        nil-sep;
   \LF-BRACES   = "{{"        nil-sep;
   \RT-BRACES   = "}}"        nil-sep;
   \LF-COMMENT  = "(*"        nil-sep;
   \RT-COMMENT  = "*)"        nil-sep;
   \CASE        = "case"      non-name-sep;
   \ELSE        = "else"      non-name-sep;
   \END         = "end"       non-name-sep;
   \FORCE       = "force"     non-name-sep;
   \IF          = "if"        non-name-sep;
   \IN          = "in"        non-name-sep;
   \INFO        = "info"      non-name-sep;
   \OPERATION   = "operation" non-name-sep;
   \REPEAT      = "repeat"    non-name-sep;
   \UNLESS      = "unless"    non-name-sep;
   \WHEN        = "when"      non-name-sep;
   \WITH        = "with"      non-name-sep;
end lexemes;

define literals
   // Used in constant construction; not lexemes in themselves.
   \DQUOTE      = "\"";
   \QUOTE       = "'";
   \BACKSLASH   = "\\";
   \HEX         = "#x";
   
   // Other literals not used in expressions.
   \TEMPLATE    = "template";
   
   // Lexemes, but need special handling.
   \TRUE        = "#t";
   \FALSE       = "#f";
end literals;


define parser lex-NAME (<lexeme>)
   rule choice(alpha-name, nonalpha-name) => token;
   inherited slot text = token;
end;

define parser lex-NUMBER (<value>)
   rule seq(many(digit), non-name-sep) => tokens;
   inherited slot text = as(<string>, tokens.first);
   inherited slot value = string-to-integer(as(<string>, tokens.first), base: 10);
   inherited slot const-value? = #t;
end;

define parser lex-HEX-NUMBER (<value>)
   rule seq(lit-HEX, many(hex-digit)) => tokens;
   inherited slot text = as(<string>, tokens[1]);
   inherited slot value = string-to-integer(as(<string>, tokens[1]), base: 16);
   inherited slot const-value? = #t;
end;

define parser lex-TRUE (<value>)
   rule lit-TRUE => token;
   inherited slot text = token;
   inherited slot value = #t;
   inherited slot const-value? = #t;
end;

define parser lex-FALSE (<value>)
   rule lit-FALSE => token;
   inherited slot text = token;
   inherited slot value = #f;
   inherited slot const-value? = #t;
end;


//
// Lexeme separators
//


define parser nil-sep
   rule nil(#t)
end;

define parser non-name-sep
   rule not-next(name-char)
end;

define parser not-sep
   rule not-next(lit-EQUAL)
end;

define parser star-sep
   rule not-next(choice(lit-RT-PAREN, name-char))
end;

define parser lf-paren-sep
   rule not-next(choice(lit-STAR, lit-LF-PAREN))
end;

define parser rt-paren-sep
   rule not-next(lit-RT-PAREN)
end;

define parser lf-angle-sep
   // Would be not-next(choice(lit-EQUAL, lit-LF-ANGLE, name-char)), but "=" and
   // "<" are name characters.
   rule not-next(name-char)
end;

define parser rt-angle-sep
   // Would be not-next(choice(lit-EQUAL, lit-RT-ANGLE, name-char)), but "=" and
   // ">" are name characters.
   rule not-next(name-char)
end;

define parser lf-brack-sep
   rule not-next(lit-LF-BRACK)
end;

define parser rt-brack-sep
   rule not-next(lit-RT-BRACK)
end;

define parser lf-brace-sep
   rule not-next(lit-LF-BRACE)
end;

define parser rt-brace-sep
   rule not-next(lit-RT-BRACE)
end;


//
// Misc low-level parsers
// 


define caching parser opt-space :: <string>
   rule opt-many(whitespace) => tokens;
   yield (tokens & as(<string>, tokens)) | ""
end;

define parser space :: <string>
   rule many(whitespace) => tokens;
   yield as(<string>, tokens)
end;

define parser alpha-name :: <string>
   rule seq(alpha-char, opt-many(name-char)) => tokens;
   yield concatenate(as(<string>, tokens[0]), tokens[1] | "")
end;

define parser nonalpha-name :: <string>
   rule seq(choice(digit, graphic-char), many(name-char)) => tokens;
   yield concatenate(as(<string>, tokens[0]), tokens[1]);
afterwards (context, tokens, result, start-pos, end-pos, fail: fail)
   check-name(result, fail)
end;

define parser name-char :: <character>
   label "name character";
   rule choice(alpha-char, graphic-char, digit, special-name-char)
      => token;
   yield token
end;

define parser-method alpha-char (stream, context)
=> (result :: false-or(<character>))
   label "alphabetic character";
   let char = read-element(stream, on-end-of-stream: #f);
   case
      char & char.alphabetic? => char;
      otherwise => #f;
   end case;
end parser-method;

define parser-method graphic-char (stream, context)
=> (result :: false-or(<character>))
   label "graphic character";
   let char = read-element(stream, on-end-of-stream: #f);
   case
      member?(char, "!&*<=>|^$%@_") => char;
      otherwise => #f;
   end case;
end parser-method;

define parser-method special-name-char (stream, context)
=> (result :: false-or(<character>))
   label "name character";
   let char = read-element(stream, on-end-of-stream: #f);
   case
      member?(char, "-+~?/") => char;
      otherwise => #f;
   end case;
end parser-method;

define parser-method digit (stream, context)
=> (result :: false-or(<character>))
   label "digit";
   let char = read-element(stream, on-end-of-stream: #f);
   case
      char & char.digit? => char;
      otherwise => #f;
   end case;
end parser-method;

define parser-method hex-digit (stream, context)
=> (result :: false-or(<character>))
   label "digit";
   let char = read-element(stream, on-end-of-stream: #f);
   case
      member?(char, "0123456789abcdefABCDEF") => char;
      otherwise => #f;
   end case;
end parser-method;

define parser-method esc-char (stream, context)
=> (result :: false-or(<character>))
   label "escape character";
   let char = read-element(stream, on-end-of-stream: #f);
   case
      member?(char, "\'\"abcfnrtABCFNRT0") => char;
      otherwise => #f;
   end case;
end parser-method;   

define parser-method whitespace (stream, context)
=> (result :: false-or(<character>))
   label "whitespace";
   let char = read-element(stream, on-end-of-stream: #f);
   case
      char & char.whitespace? => char;
      otherwise => #f;
   end case;
end parser-method;

define parser-method char (stream, context)
=> (result :: false-or(<character>))
   label "character";
   read-element(stream, on-end-of-stream: #f);
end parser-method;

define parser-method eof (stream, context)
=> (result :: <boolean>, success? :: <boolean>)
   label "end of template document";
   values(#f, stream.stream-at-end?)
end parser-method;


//
// Lexeme parser helper methods
//


define method read-matching-string (stream, string :: <string>)
=> (str :: false-or(<string>))
   let in-str = read(stream, string.size, on-end-of-stream: #f);
   case
      case-insensitive-equal?(in-str, string) => string;
      otherwise => #f;
   end case
end method;


define method check-name (name :: <string>, fail :: <function>) => ()
   unless (any?(alphabetic?, name))
      fail(make(<parse-failure>, expected: "alphabetic character in name"))
   end unless
end method;

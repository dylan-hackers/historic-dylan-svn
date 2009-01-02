module: dylan-parser
synopsis: The Dylan lexical grammar, as defined in the DRM, with some omissions.

define class <lexeme> (<token>)
   slot lexeme-doc :: false-or(<doc-comment-token>) = #f;
end class;

define method concatenated-strings (s :: <string>) => (s :: <string>)
   s
end method;

define method concatenated-strings (s :: <sequence>) => (s :: <string>)
   apply(concatenate, "", s)
end method;

define method translate-escape-char (s :: <string>) => (s :: <string>)
   select (s by \=)
      "a" => "\<07>";
      "b" => "\<08>";
      "e" => "\<1b>";
      "f" => "\<0c>";
      "n" => "\<0a>";
      "r" => "\<0d>";
      "t" => "\<09>";
      "0" => "\<00>";
      otherwise => s;
   end select;
end method;

define method translate-escape-code (s :: <string>) => (s :: <string>)
   let code = string-to-integer(s, base: 16);
   format-to-string("%c", as(<character>, code));
end method;

define inline method adjust-lexeme-start
   (lexeme :: <lexeme>, whitespace :: false-or(<whitespace-token>))
=> ()
   if (whitespace)
      lexeme.parse-start := whitespace.parse-end;
   end if;
end method;


//
// Lexical Syntax
//
// I wish oh I wish I could write a macro to simplify <lexeme> and token separator
// boilerplate, but I can't get it to work. Probably an issue with Gwydion Dylan.

// Token separators keep tokens that begin identically from being confused.
define parser word-sep              rule not-next(any-character) end;
define parser pound-word-sep        rule nil(#f) end;
define parser colon-sep             rule not-next(choice(colon, equal)) end;  
define parser double-colon-sep      rule nil(#f) end;
define parser arrow-sep             rule nil(#f) end;
define parser double-question-sep   rule nil(#f) end;
define parser question-equal-sep    rule nil(#f) end;
define parser question-sep          rule not-next(choice(equal, question)) end;
define parser ellipsis-sep          rule nil(#f) end;
define parser double-pound-sep      rule nil(#f) end;
// unused define parser pound-sep             rule not-next(choice(alphabetic-character, pound, lf-paren, lf-brack)) end;
// unused define parser double-slash-sep      rule nil(#f) end;
// unused define parser lf-comment-sep        rule nil(#f) end;         
// unused define parser rt-comment-sep        rule not-next(word-character-not-alphabetic) end;
define parser empty-list-sep        rule nil(#f) end;
// unused define parser bin-sep               rule nil(#f) end;
// unused define parser oct-sep               rule nil(#f) end;
// unused define parser hex-sep               rule nil(#f) end;
define parser lf-list-sep           rule nil(#f) end;
define parser lf-vector-sep         rule nil(#f) end;
define parser identical-sep         rule nil(#f) end;
define parser not-identical-sep     rule nil(#f) end;
define parser not-equal-sep         rule nil(#f) end;
define parser lt-equal-sep          rule nil(#f) end;      
define parser gt-equal-sep          rule nil(#f) end;
define parser bind-sep              rule nil(#f) end;
define parser plus-sep              rule nil(#f) end;
define parser minus-sep             rule nil(#f) end;
define parser star-sep              rule not-next(choice(any-character, slash)) end;
define parser slash-sep             rule not-next(slash) end;
define parser caret-sep             rule not-next(any-character) end;
define parser equal-sep             rule not-next(gt) end;
define parser lt-sep                rule not-next(choice(any-character, equal)) end;
define parser gt-sep                rule not-next(choice(any-character, equal)) end;
define parser amp-sep               rule not-next(any-character) end;
define parser vert-bar-sep          rule not-next(any-character) end;
define parser not-sep               rule not-next(equal) end;
define parser apos-sep              rule nil(#f) end;
// unused define parser backslash-sep         rule nil(#f) end;
define parser quote-sep             rule nil(#f) end;
define parser period-sep            rule not-next(seq(period, period)) end;
// unused define parser exp-sep               rule not-next(any-character) end;
define parser comma-sep             rule nil(#f) end;
define parser semicolon-sep         rule nil(#f) end;
// unused define parser esc-char-sep          rule not-next(any-character) end;
define parser lf-paren-sep          rule nil(#f) end;
define parser rt-paren-sep          rule nil(#f) end;
define parser lf-brack-sep          rule nil(#f) end;
define parser rt-brack-sep          rule nil(#f) end;
define parser lf-brace-sep          rule nil(#f) end;
define parser rt-brace-sep          rule nil(#f) end;
// unused define parser bin-digit-sep         rule not-next(binary-digit) end;
// unused define parser oct-digit-sep         rule not-next(octal-digit)  end;
// unused define parser dec-digit-sep         rule not-next(dec-digit)    end;
// unused define parser hex-digit-sep         rule not-next(hex-digit)    end;
   

//
// Tokens
//

define parser lex-TOKEN
   rule choice(lex-NAME, lex-SYMBOL, lex-NUMBER, lex-CHARACTER-LITERAL,
               lex-STRING, lex-UNARY-OPERATOR, lex-BINARY-OPERATOR, punctuation,
               lex-POUND-WORD);
end;

define parser punctuation
   rule choice(lex-LF-BRACK, lex-RT-BRACK, lex-DOUBLE-COLON, /*unused lex-MINUS,*/
               lex-IDENTICAL, lex-ARROW, lex-LF-LIST, lex-LF-VECTOR,
               lex-DOUBLE-POUND, lex-DOUBLE-QUESTION, lex-QUESTION-EQUAL,
               lex-LF-PAREN, lex-RT-PAREN, lex-COMMA, lex-PERIOD,
               lex-SEMICOLON, lex-LF-BRACE, lex-RT-BRACE, lex-EQUAL,
               lex-QUESTION, lex-ELLIPSIS);
end;

define parser pound-word :: <string>
   rule choice(true, false, next, rest, key, all-keys, include) => token;
   yield token;
end;

define parser lex-POUND-WORD (<lexeme>)
   rule seq(opt-whitespace, pound-word, req-next(pound-word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

// Tokens directly used in phrase grammar.

define parser lex-DEFINE (<lexeme>)
   rule seq(opt-whitespace, lit-define, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-END (<lexeme>)
   rule seq(opt-whitespace, lit-end, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-HANDLER (<lexeme>)
   rule seq(opt-whitespace, lit-handler, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-LET (<lexeme>)
   rule seq(opt-whitespace, lit-let, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-LOCAL (<lexeme>)
   rule seq(opt-whitespace, lit-local, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-MACRO (<lexeme>)
   rule seq(opt-whitespace, lit-macro, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-METHOD (<lexeme>) 
   rule seq(opt-whitespace, lit-method, req-next(word-sep)) => tokens; 
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-OTHERWISE (<lexeme>) 
   rule seq(opt-whitespace, lit-otherwise, req-next(word-sep)) => tokens; 
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-DOUBLE-COLON (<lexeme>)
   rule seq(opt-whitespace, double-colon, req-next(double-colon-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-ARROW (<lexeme>) 
   rule seq(opt-whitespace, arrow, req-next(arrow-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-DOUBLE-QUESTION (<lexeme>)
   rule seq(opt-whitespace, double-question, req-next(double-question-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-QUESTION-EQUAL (<lexeme>) 
   rule seq(opt-whitespace, question-equal, req-next(question-equal-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-QUESTION (<lexeme>) 
   rule seq(opt-whitespace, question, req-next(question-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-ELLIPSIS (<lexeme>) 
   rule seq(opt-whitespace, ellipsis, req-next(ellipsis-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-DOUBLE-POUND (<lexeme>) 
   rule seq(opt-whitespace, double-pound, req-next(double-pound-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-EMPTY-LIST (<lexeme>) 
   rule seq(opt-whitespace, empty-list, req-next(empty-list-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-TRUE (<lexeme>) 
   rule seq(opt-whitespace, true, req-next(pound-word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-FALSE (<lexeme>) 
   rule seq(opt-whitespace, false, req-next(pound-word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-NEXT (<lexeme>) 
   rule seq(opt-whitespace, next, req-next(pound-word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-REST (<lexeme>) 
   rule seq(opt-whitespace, rest, req-next(pound-word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-KEY (<lexeme>) 
   rule seq(opt-whitespace, key, req-next(pound-word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-ALL-KEYS (<lexeme>) 
   rule seq(opt-whitespace, all-keys, req-next(pound-word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define caching parser lex-LF-LIST (<lexeme>) 
   rule seq(opt-whitespace, lf-list, req-next(lf-list-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-LF-VECTOR (<lexeme>) 
   rule seq(opt-whitespace, lf-vector, req-next(lf-vector-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-IDENTICAL (<lexeme>) 
   rule seq(opt-whitespace, identical, req-next(identical-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-EQUAL (<lexeme>) 
   rule seq(opt-whitespace, equal, req-next(equal-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-PERIOD (<lexeme>) 
   rule seq(opt-whitespace, period, req-next(period-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define caching parser lex-COMMA (<lexeme>) 
   rule seq(opt-whitespace, comma, req-next(comma-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define caching parser lex-SEMICOLON (<lexeme>) 
   rule seq(opt-whitespace, semicolon, req-next(semicolon-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-LF-PAREN (<lexeme>) 
   rule seq(opt-whitespace, lf-paren, req-next(lf-paren-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-RT-PAREN (<lexeme>) 
   rule seq(opt-whitespace, rt-paren, req-next(rt-paren-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-LF-BRACK (<lexeme>) 
   rule seq(opt-whitespace, lf-brack, req-next(lf-brack-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-RT-BRACK (<lexeme>) 
   rule seq(opt-whitespace, rt-brack, req-next(rt-brack-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-LF-BRACE (<lexeme>) 
   rule seq(opt-whitespace, lf-brace, req-next(lf-brace-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-RT-BRACE (<lexeme>) 
   rule seq(opt-whitespace, rt-brace, req-next(rt-brace-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

// Tokens used in definitions.

define parser lex-ALL (<lexeme>)
   rule seq(opt-whitespace, lit-all, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-CREATE (<lexeme>)
   rule seq(opt-whitespace,
            choice(seq(nil(#f), nil(#f), lit-create, colon, req-next(colon-sep)),
                   seq(pound, quote, lit-create, quote, req-next(quote-sep))))
   => tokens;
   slot value :: <string> = tokens[1][2];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-EXCLUDE (<lexeme>)
   rule seq(opt-whitespace,
            choice(seq(nil(#f), nil(#f), lit-exclude, colon, req-next(colon-sep)),
                   seq(pound, quote, lit-exclude, quote, req-next(quote-sep))))
   => tokens;
   slot value :: <string> = tokens[1][2];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-EXPORT (<lexeme>)
   rule seq(opt-whitespace,
            choice(seq(nil(#f), nil(#f), lit-export, colon, req-next(colon-sep)),
                   seq(pound, quote, lit-export, quote, req-next(quote-sep))))
   => tokens;
   slot value :: <string> = tokens[1][2];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-IMPORT (<lexeme>)
   rule seq(opt-whitespace,
            choice(seq(nil(#f), nil(#f), lit-import, colon, req-next(colon-sep)),
                   seq(pound, quote, lit-import, quote, req-next(quote-sep))))
   => tokens;
   slot value :: <string> = tokens[1][2];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-INHERITED (<lexeme>)
   rule seq(opt-whitespace, lit-inherited, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-INIT-FUNCTION (<lexeme>)
   rule seq(opt-whitespace,
            choice(seq(nil(#f), nil(#f), lit-init-function, colon, req-next(colon-sep)),
                   seq(pound, quote, lit-init-function, quote, req-next(quote-sep))))
   => tokens;
   slot value :: <string> = tokens[1][2];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-INIT-KEYWORD (<lexeme>)
   rule seq(opt-whitespace,
            choice(seq(nil(#f), nil(#f), lit-init-keyword, colon, req-next(colon-sep)),
                   seq(pound, quote, lit-init-keyword, quote, req-next(quote-sep))))
   => tokens;
   slot value :: <string> = tokens[1][2];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-INIT-VALUE (<lexeme>)
   rule seq(opt-whitespace,
            choice(seq(nil(#f), nil(#f), lit-init-value, colon, req-next(colon-sep)),
                   seq(pound, quote, lit-init-value, quote, req-next(quote-sep))))
   => tokens;
   slot value :: <string> = tokens[1][2];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-KEYWORD (<lexeme>)
   rule seq(opt-whitespace, lit-keyword, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-PREFIX (<lexeme>)
   rule seq(opt-whitespace,
            choice(seq(nil(#f), nil(#f), lit-prefix, colon, req-next(colon-sep)),
                   seq(pound, quote, lit-prefix, quote, req-next(quote-sep))))
   => tokens;
   slot value :: <string> = tokens[1][2];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-RENAME (<lexeme>)
   rule seq(opt-whitespace,
            choice(seq(nil(#f), nil(#f), lit-rename, colon, req-next(colon-sep)),
                   seq(pound, quote, lit-rename, quote, req-next(quote-sep))))
   => tokens;
   slot value :: <string> = tokens[1][2];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-REQUIRED-INIT-KEYWORD (<lexeme>)
   rule seq(opt-whitespace,
            choice(seq(nil(#f), nil(#f), lit-req-init-keyword, colon, req-next(colon-sep)),
                   seq(pound, quote, lit-req-init-keyword, quote, req-next(quote-sep))))
   => tokens;
   slot value :: <string> = tokens[1][2];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-REQUIRED (<lexeme>)
   rule seq(opt-whitespace, lit-required, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-SEALED (<lexeme>)
   rule seq(opt-whitespace, lit-sealed, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-SETTER (<lexeme>)
   rule seq(opt-whitespace,
            choice(seq(nil(#f), nil(#f), lit-setter, colon, req-next(colon-sep)),
                   seq(pound, quote, lit-setter, quote, req-next(quote-sep))))
   => tokens;
   slot value :: <string> = tokens[1][2];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-SLOT (<lexeme>)
   rule seq(opt-whitespace, lit-slot, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-TYPE (<lexeme>)
   rule seq(opt-whitespace,
            choice(seq(nil(#f), nil(#f), lit-type, colon, req-next(colon-sep)),
                   seq(pound, quote, lit-type, quote, req-next(quote-sep))))
   => tokens;
   slot value :: <string> = tokens[1][2];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-USE (<lexeme>)
   rule seq(opt-whitespace,
            choice(seq(nil(#f), nil(#f), lit-use, colon, req-next(colon-sep)),
                   seq(pound, quote, lit-use, quote, req-next(quote-sep))))
   => tokens;
   slot value :: <string> = tokens[1][2];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

//
// Reserved Words
//

define parser reserved-word :: <string>
   label "reserved word";
   rule choice(core-word, begin-word, function-word,
               define-body-word, define-list-word)
   => token;
   yield token;
end;

define parser core-word :: <string>
   rule seq(choice(lit-define, lit-end, lit-handler, lit-let, lit-local, lit-macro,
                   lit-otherwise),
            req-next(word-sep))
   => tokens;
   yield tokens[0];
end;

define parser begin-word :: <string>
   rule seq(choice(lit-begin, lit-block, lit-case, lit-for, lit-if, lit-method,
                   lit-select, lit-unless, lit-until, lit-while),
            req-next(word-sep))
   => tokens;
   yield tokens[0];
end;

define parser lex-BEGIN-WORD (<lexeme>)
   rule seq(opt-whitespace, begin-word)
   => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser function-word :: <string>
   rule seq(choice(), req-next(word-sep)) => tokens;
   yield tokens[0];
end;

define parser lex-FUNCTION-WORD (<lexeme>)
   rule seq(opt-whitespace, function-word)
   => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser define-body-word :: <string>
   rule seq(choice(lit-class, lit-library, lit-method, lit-function, lit-module),
            req-next(word-sep))
   => tokens;
   yield tokens[0];
end;

define parser lex-DEFINE-BODY-WORD (<lexeme>)
   rule seq(opt-whitespace, define-body-word)
   => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser define-list-word :: <string>
   rule seq(choice(lit-constant, lit-variable, lit-generic, lit-domain),
            req-next(word-sep))
   => tokens;
   yield tokens[0];
end;

define parser lex-DEFINE-LIST-WORD (<lexeme>)
   rule seq(opt-whitespace, define-list-word)
   => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

//
// Reserved words used in definitions.
//

define parser lex-CLASS (<lexeme>)
   rule seq(opt-whitespace, lit-class, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-CONSTANT (<lexeme>)
   rule seq(opt-whitespace, lit-constant, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-DOMAIN (<lexeme>)
   rule seq(opt-whitespace, lit-domain, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-FUNCTION (<lexeme>)
   rule seq(opt-whitespace, lit-function, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-GENERIC (<lexeme>)
   rule seq(opt-whitespace, lit-generic, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-LIBRARY (<lexeme>)
   rule seq(opt-whitespace, lit-library, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-MODULE (<lexeme>)
   rule seq(opt-whitespace, lit-module, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-VARIABLE (<lexeme>)
   rule seq(opt-whitespace, lit-variable, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

//
// Names, Symbols and Keywords
//

define parser name :: <string>
   rule choice(word, seq(backslash, word), operator-name) => token;
   yield token.concatenated-strings;
end;

define parser lex-NAME (<lexeme>)
   label "name";
   rule seq(opt-whitespace, name, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser unreserved-name :: <string>
   rule seq(not-next(reserved-word), name) => tokens;
   yield tokens[1];
end;

define parser lex-UNRESERVED-NAME (<lexeme>)
   label "name";
   rule seq(opt-whitespace, unreserved-name, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser ordinary-name :: <string>
   rule choice(unreserved-name, define-body-word, define-list-word) => token;
   yield token;
end;

define parser lex-ORDINARY-NAME (<lexeme>)
   label "name";
   rule seq(opt-whitespace, ordinary-name, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-CONSTRAINED-NAME (<lexeme>)
   label "constrained name";
   rule seq(opt-whitespace,
            choice(seq(name, colon, word, req-next(word-sep)),
                   seq(name, colon, binary-operator-with-sep),
                   seq(colon, word, req-next(word-sep))))
   => tokens;
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser operator-name :: <string>
   rule seq(backslash, choice(unary-operator, binary-operator)) => tokens;
   yield tokens.concatenated-strings;
end;

define parser macro-name :: <string>
   rule choice(ordinary-name, begin-word, function-word) => token;
   yield token;
end;

define parser lex-MACRO-NAME (<lexeme>)
   label "name";
   rule seq(opt-whitespace, macro-name, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-NAME-NOT-END (<lexeme>)
   label "name";
   rule seq(opt-whitespace,
            choice(macro-name, lit-define, lit-handler, lit-let,
                   lit-local, lit-macro, lit-otherwise),
            req-next(word-sep))
   => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser lex-SYMBOL (<lexeme>)
   label "symbol";
   rule seq(opt-whitespace,
            choice(seq(nil(#f), word, colon, req-next(colon-sep)),
                   seq(pound, string, req-next(quote-sep))))
   => tokens;
   slot value :: <string> = tokens[1][1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser word :: <string>
   rule choice(leading-alphabetic,
               seq(leading-numeric, alphabetic-character, leading-alphabetic),
               seq(leading-graphic, leading-alphabetic))
   => token;
   yield token.concatenated-strings;
end;

define parser leading-alphabetic :: <string>
   rule seq(alphabetic-character, opt-many(any-character)) => tokens;
   yield apply(concatenate, tokens[0], tokens[1] | #[""]);
end;

define parser leading-numeric :: <string>
   rule seq(numeric-character, opt-many(word-character-not-double-alphabetic))
   => tokens;
   yield apply(concatenate, tokens[0], tokens[1] | #[""]);
end;

define parser leading-graphic :: <string>
   rule seq(graphic-character, opt-many(word-character-not-alphabetic)) => tokens;
   yield apply(concatenate, tokens[0], tokens[1] | #[""]);
end;

define parser word-character-not-alphabetic :: <string>
   rule choice(numeric-character, graphic-character, special-character) => token;
   yield token;
end;

define parser word-character-not-double-alphabetic :: <string>
   rule choice(seq(alphabetic-character, word-character-not-alphabetic),
               numeric-character,
               graphic-character,
               special-character)
   => token;
   yield token.concatenated-strings;
end;

define caching parser any-character :: <string>
   rule choice(alphabetic-character,
               numeric-character,
               graphic-character,
               special-character)
   => token;
   yield token;
end;

define parser-method alphabetic-character (stream, context)
=> (char :: false-or(<string>),
    succ? :: <boolean>, err :: false-or(<parse-failure>))
   label "alphabetic character";
   let char = read-element(stream, on-end-of-stream: #f);
   case
      char & char.alphabetic? => values(format-to-string("%c", char), #t, #f);
      ~char =>                   values(#f, #f, $eof);
      otherwise =>               values(#f, #f, #f);
   end case;
end parser-method;

define parser-method numeric-character (stream, context)
=> (char :: false-or(<string>),
    succ? :: <boolean>, err :: false-or(<parse-failure>))
   label "numeric character";
   let char = read-element(stream, on-end-of-stream: #f);
   case
      char & char.digit? =>   values(format-to-string("%c", char), #t, #f);
      ~char =>                values(#f, #f, $eof);
      otherwise =>            values(#f, #f, #f);
   end case;
end parser-method;

define lexical-parsers
   graphic-character       in "!&*<>|^$%@_"  label "graphic character";
   special-character       in "-+~?/="       label "graphic character";
end;

//
// Operators
//

define lexical-parsers
   unary-operator          in "-~"           label "unary operator";
end;

define parser unary-operator-with-sep :: <string>
   rule choice(seq(minus,  req-next(minus-sep)),
               seq(not,    req-next(not-sep  )))
   => token;
   yield token[0];
end;

define parser lex-UNARY-OPERATOR (<lexeme>)
   label "unary operator";
   rule seq(opt-whitespace, unary-operator-with-sep) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser binary-operator :: <string>
   label "binary operator";
   rule choice(identical, not-identical, not-equal, lt-equal, gt-equal, bind,
               plus, minus, star, slash, caret, equal, lt, gt, amp, vert-bar)
   => token;
   yield token;
end;

define parser binary-operator-with-sep :: <string>
   rule choice(seq(identical,       req-next(identical-sep     )),
               seq(not-identical,   req-next(not-identical-sep )),
               seq(not-equal,       req-next(not-equal-sep     )),
               seq(lt-equal,        req-next(lt-equal-sep      )),
               seq(gt-equal,        req-next(gt-equal-sep      )),
               seq(bind,            req-next(bind-sep          )),
               seq(plus,            req-next(plus-sep          )),
               seq(minus,           req-next(minus-sep         )),
               seq(star,            req-next(star-sep          )),
               seq(slash,           req-next(slash-sep         )),
               seq(caret,           req-next(caret-sep         )),
               seq(equal,           req-next(equal-sep         )),
               seq(lt,              req-next(lt-sep            )),
               seq(gt,              req-next(gt-sep            )),
               seq(amp,             req-next(amp-sep           )),
               seq(vert-bar,        req-next(vert-bar-sep      )))
   => token;
   yield token[0];
end;

define parser lex-BINARY-OPERATOR (<lexeme>)
   label "binary operator";
   rule seq(opt-whitespace, binary-operator-with-sep) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

//
// Character and String Literals
//

define parser lex-CHARACTER-LITERAL (<lexeme>)
   label "character literal";
   rule seq(opt-whitespace, apos, character, apos, req-next(apos-sep)) => tokens;
   slot value :: <string> = tokens[2];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser character :: <string>
   rule choice(seq(not-next(apos), not-next(backslash), printing-character),
               seq(backslash, escape-character))
   => token;
   yield token.last;
end;

define parser string :: <string>
   rule seq(quote, opt-many(string-character), quote) => tokens;
   yield tokens[1].concatenated-strings;
end;

define parser lex-STRING (<lexeme>)
   label "string literal";
   rule seq(opt-whitespace, string, req-next(quote-sep)) => tokens;
   slot value :: <string> = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser string-character :: <string>
   rule choice(seq(not-next(quote), not-next(backslash), printing-character),
               seq(backslash, escape-character))
   => token;
   yield token.last;
end;

define parser escape-character :: <string>
   rule choice(seq(nil(#"char"), esc-char),
               seq(nil(#"code"), lt, many(hex-digit), gt))
   => token;
   yield select (token[0])
            #"char" => translate-escape-char(token[1]);
            #"code" => translate-escape-code(token[2].concatenated-strings);
         end select;
end;

//
// Numbers
//

define parser lex-NUMBER (<lexeme>)
   label "number";
   rule seq(opt-whitespace, choice(integer, ratio, floating-point))
   => tokens;
   slot value = tokens[1];
   inherited slot lexeme-doc = last-whitespace-docs(tokens[0]);
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-lexeme-start(value, tokens[0]);
end;

define parser integer (<token>)
   rule choice(seq(nil(#"bin"), binary-integer),
               seq(nil(#"oct"), octal-integer),
               seq(nil(#"dec"), opt(sign), decimal-integer),
               seq(nil(#"hex"), hex-integer))
   => token;
   slot base :: <symbol> = token[0];
   slot value :: <string>
      = if (token[0] = #"dec") concatenate(token[1] | "", token[2]) else token[1] end;
end;

define parser binary-integer :: <string>
   rule seq(bin, many(binary-digit)) => tokens;
   yield tokens[1].concatenated-strings;
end;

define parser octal-integer :: <string>
   rule seq(oct, many(octal-digit)) => tokens;
   yield tokens[1].concatenated-strings;
end;

define caching parser decimal-integer :: <string>
   rule many(decimal-digit) => tokens;
   yield tokens.concatenated-strings;
end;

define parser hex-integer :: <string>
   rule seq(hex, many(hex-digit)) => tokens;
   yield tokens[1].concatenated-strings;
end;

define lexical-parsers
   binary-digit   in "01"                 label "binary digit";
   octal-digit    in "01234567"           label "octal digit";
   decimal-digit  in "0123456789"         label "decimal digit";
   hex-digit      in "0123456789abcdef"   label "hexadecimal digit";
end lexical-parsers;

define parser ratio (<token>)
   rule seq(opt(sign), decimal-integer, slash, decimal-integer) => tokens;
   slot sign :: <string> = tokens[0] | "+";
   slot numerator :: <string> = tokens[1];
   slot denominator :: <string> = tokens[3];
end;

define parser floating-point (<token>)
   rule seq(opt(sign),
            choice(seq(nil(#"form1"), opt(decimal-integer), period, decimal-integer, opt(exponent)),
                   seq(nil(#"form2"), decimal-integer, period, req-next(period-sep), opt(decimal-integer), opt(exponent)),
                   seq(nil(#"form3"), decimal-integer, exponent)))
   => tokens;
   slot mantissa-sign :: <string> = tokens[0] | "+";
   slot mantissa-int :: <string> = tokens[1][1] | "0";
   slot mantissa-frac :: <string> = select (tokens[1][0])
                                       #"form1" => tokens[1][3];
                                       #"form2" => tokens[1][4];
                                       #"form3" => "0";
                                    end select;
   slot exp-sign :: <string> = (tokens[1].last & tokens[1].last.sign) | "+";
   slot exp-value :: <string> = (tokens[1].last & tokens[1].last.value) | "0";
end;

define parser exponent (<token>)
   rule seq(exp, opt(sign), decimal-integer) => tokens;
   slot sign :: <string> = tokens[1] | "+";
   slot value :: <string> = tokens[2];
end;

define lexical-parsers
   sign  in "+-"  label "\"+\" or \"-\"";
end lexical-parsers;

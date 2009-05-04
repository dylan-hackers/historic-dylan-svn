module: dylan-parser
synopsis: The Dylan lexical grammar, as defined in the DRM, with some omissions.

define class <lexeme> (<token>)
   slot lexeme-doc :: false-or(<markup-content-token>) = #f;
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

define inline method consume-last-whitespace-doc (value :: <lexeme>, context)
=> ()
   value.lexeme-doc := context.last-whitespace-doc;
   context.last-whitespace-doc := #f;
end method;

define inline method adjust-token-end
   (value :: <token>, tokens :: <sequence>)
=> ()
   value.parse-end := tokens.last.parse-start;
end method;


//
// Lexical Syntax
//
// I wish oh I wish I could write a macro to simplify <lexeme> and token separator
// boilerplate, but I can't get it to work. Probably an issue with Gwydion Dylan.

// Token separators keep tokens that begin identically from being confused.
define caching parser word-sep ()       rule seq(not-next(any-character), opt-whitespace) => tokens end;
define parser pound-word-sep ()         rule opt-whitespace => tokens end;
define parser colon-sep ()              rule seq(not-next(choice(colon, equal)), opt-whitespace) => tokens end;  
define parser double-colon-sep ()       rule opt-whitespace => tokens end;
define parser arrow-sep ()              rule opt-whitespace => tokens end;
define parser double-question-sep ()    rule opt-whitespace => tokens end;
define parser question-equal-sep ()     rule opt-whitespace => tokens end;
define parser question-sep ()           rule seq(not-next(choice(equal, question)), opt-whitespace) => tokens end;
define parser ellipsis-sep ()           rule opt-whitespace => tokens end;
define parser double-pound-sep ()       rule opt-whitespace => tokens end;
// unused define parser pound-sep ()              rule seq(not-next(choice(alphabetic-character, pound, lf-paren, lf-brack)), opt-whitespace) => tokens end;
// unused define parser double-slash-sep ()       rule opt-whitespace => tokens end;
// unused define parser lf-comment-sep ()         rule opt-whitespace => tokens end;         
// unused define parser rt-comment-sep ()         rule seq(not-next(word-character-not-alphabetic), opt-whitespace) => tokens end;
define parser empty-list-sep ()         rule opt-whitespace => tokens end;
// unused define parser bin-sep ()                rule opt-whitespace => tokens end;
// unused define parser oct-sep ()                rule opt-whitespace => tokens end;
// unused define parser hex-sep ()                rule opt-whitespace => tokens end;
define parser lf-list-sep ()            rule opt-whitespace => tokens end;
define parser lf-vector-sep ()          rule opt-whitespace => tokens end;
define parser identical-sep ()          rule opt-whitespace => tokens end;
define parser not-identical-sep ()      rule opt-whitespace => tokens end;
define parser not-equal-sep ()          rule opt-whitespace => tokens end;
define parser lt-equal-sep ()           rule opt-whitespace => tokens end;      
define parser gt-equal-sep ()           rule opt-whitespace => tokens end;
define parser bind-sep ()               rule opt-whitespace => tokens end;
define parser plus-sep ()               rule opt-whitespace => tokens end;
define parser minus-sep ()              rule opt-whitespace => tokens end;
define parser star-sep ()               rule seq(not-next(choice(any-character, slash)), opt-whitespace) => tokens end;
define parser slash-sep ()              rule seq(not-next(slash), opt-whitespace) => tokens end;
define parser caret-sep ()              rule seq(not-next(any-character), opt-whitespace) => tokens end;
define parser equal-sep ()              rule seq(not-next(gt), opt-whitespace) => tokens end;
define parser lt-sep ()                 rule seq(not-next(choice(any-character, equal)), opt-whitespace) => tokens end;
define parser gt-sep ()                 rule seq(not-next(choice(any-character, equal)), opt-whitespace) => tokens end;
define parser amp-sep ()                rule seq(not-next(any-character), opt-whitespace) => tokens end;
define parser vert-bar-sep ()           rule seq(not-next(any-character), opt-whitespace) => tokens end;
define parser not-sep ()                rule seq(not-next(equal), opt-whitespace) => tokens end;
define parser apos-sep ()               rule opt-whitespace => tokens end;
// unused define parser backslash-sep ()          rule opt-whitespace => tokens end;
define parser quote-sep ()              rule opt-whitespace => tokens end;
define parser period-sep ()             rule seq(not-next(seq(period, period)), opt-whitespace) => tokens end;
// unused define parser exp-sep ()                rule seq(not-next(any-character), opt-whitespace) => tokens end;
define parser comma-sep ()              rule opt-whitespace => tokens end;
define parser semicolon-sep ()          rule opt-whitespace => tokens end;
// unused define parser esc-char-sep ()           rule seq(not-next(any-character), opt-whitespace) => tokens end;
define parser lf-paren-sep ()           rule opt-whitespace => tokens end;
define parser rt-paren-sep ()           rule opt-whitespace => tokens end;
define parser lf-brack-sep ()           rule opt-whitespace => tokens end;
define parser rt-brack-sep ()           rule opt-whitespace => tokens end;
define parser lf-brace-sep ()           rule opt-whitespace => tokens end;
define parser rt-brace-sep ()           rule opt-whitespace => tokens end;
// unused define parser bin-digit-sep ()          rule seq(not-next(binary-digit), opt-whitespace) => tokens end;
// unused define parser oct-digit-sep ()          rule seq(not-next(octal-digit), opt-whitespace)  => tokens end;
// unused define parser dec-digit-sep ()          rule seq(not-next(dec-digit), opt-whitespace)    => tokens end;
// unused define parser hex-digit-sep ()          rule seq(not-next(hex-digit), opt-whitespace)    => tokens end;
   

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
   rule seq(pound-word, pound-word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

// Tokens directly used in phrase grammar.

define parser lex-DEFINE (<lexeme>)
   rule seq(lit-define, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-END (<lexeme>)
   rule seq(lit-end, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-HANDLER (<lexeme>)
   rule seq(lit-handler, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-LET (<lexeme>)
   rule seq(lit-let, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-LOCAL (<lexeme>)
   rule seq(lit-local, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-MACRO (<lexeme>)
   rule seq(lit-macro, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-METHOD (<lexeme>) 
   rule seq(lit-method, word-sep) => tokens; 
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-OTHERWISE (<lexeme>) 
   rule seq(lit-otherwise, word-sep) => tokens; 
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-DOUBLE-COLON (<lexeme>)
   rule seq(double-colon, double-colon-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-ARROW (<lexeme>) 
   rule seq(arrow, arrow-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-DOUBLE-QUESTION (<lexeme>)
   rule seq(double-question, double-question-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-QUESTION-EQUAL (<lexeme>) 
   rule seq(question-equal, question-equal-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-QUESTION (<lexeme>) 
   rule seq(question, question-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-ELLIPSIS (<lexeme>) 
   rule seq(ellipsis, ellipsis-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-DOUBLE-POUND (<lexeme>) 
   rule seq(double-pound, double-pound-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-EMPTY-LIST (<lexeme>) 
   rule seq(empty-list, empty-list-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-TRUE (<lexeme>) 
   rule seq(true, pound-word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-FALSE (<lexeme>) 
   rule seq(false, pound-word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-NEXT (<lexeme>) 
   rule seq(next, pound-word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-REST (<lexeme>) 
   rule seq(rest, pound-word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-KEY (<lexeme>) 
   rule seq(key, pound-word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-ALL-KEYS (<lexeme>) 
   rule seq(all-keys, pound-word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-LF-LIST (<lexeme>) 
   rule seq(lf-list, lf-list-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-LF-VECTOR (<lexeme>) 
   rule seq(lf-vector, lf-vector-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-IDENTICAL (<lexeme>) 
   rule seq(identical, identical-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-EQUAL (<lexeme>) 
   rule seq(equal, equal-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-PERIOD (<lexeme>) 
   rule seq(period, period-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-COMMA (<lexeme>) 
   rule seq(comma, comma-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-SEMICOLON (<lexeme>) 
   rule seq(semicolon, semicolon-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-LF-PAREN (<lexeme>) 
   rule seq(lf-paren, lf-paren-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-RT-PAREN (<lexeme>) 
   rule seq(rt-paren, rt-paren-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-LF-BRACK (<lexeme>) 
   rule seq(lf-brack, lf-brack-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-RT-BRACK (<lexeme>) 
   rule seq(rt-brack, rt-brack-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-LF-BRACE (<lexeme>) 
   rule seq(lf-brace, lf-brace-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-RT-BRACE (<lexeme>) 
   rule seq(rt-brace, rt-brace-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

// Tokens used in definitions.

define parser lex-ALL (<lexeme>)
   rule seq(lit-all, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-CREATE (<lexeme>)
   rule seq(lit-create, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-EXCLUDE-SYM (<lexeme>)
   rule choice(seq(nil(#f), nil(#f), lit-exclude, colon, colon-sep),
               seq(pound, quote, lit-exclude, quote, quote-sep))
   => tokens;
   slot value :: <string> = tokens[2];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-EXPORT (<lexeme>)
   rule seq(lit-export, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-EXPORT-SYM (<lexeme>)
   rule choice(seq(nil(#f), nil(#f), lit-export, colon, colon-sep),
               seq(pound, quote, lit-export, quote, quote-sep))
   => tokens;
   slot value :: <string> = tokens[2];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-IMPORT-SYM (<lexeme>)
   rule choice(seq(nil(#f), nil(#f), lit-import, colon, colon-sep),
               seq(pound, quote, lit-import, quote, quote-sep))
   => tokens;
   slot value :: <string> = tokens[2];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-INHERITED (<lexeme>)
   rule seq(lit-inherited, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-INIT-FUNCTION-SYM (<lexeme>)
   rule choice(seq(nil(#f), nil(#f), lit-init-function, colon, colon-sep),
               seq(pound, quote, lit-init-function, quote, quote-sep))
   => tokens;
   slot value :: <string> = tokens[2];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-INIT-KEYWORD-SYM (<lexeme>)
   rule choice(seq(nil(#f), nil(#f), lit-init-keyword, colon, colon-sep),
               seq(pound, quote, lit-init-keyword, quote, quote-sep))
   => tokens;
   slot value :: <string> = tokens[2];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-INIT-VALUE-SYM (<lexeme>)
   rule choice(seq(nil(#f), nil(#f), lit-init-value, colon, colon-sep),
               seq(pound, quote, lit-init-value, quote, quote-sep))
   => tokens;
   slot value :: <string> = tokens[2];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-KEYWORD (<lexeme>)
   rule seq(lit-keyword, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-PREFIX-SYM (<lexeme>)
   rule choice(seq(nil(#f), nil(#f), lit-prefix, colon, colon-sep),
               seq(pound, quote, lit-prefix, quote, quote-sep))
   => tokens;
   slot value :: <string> = tokens[2];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-RENAME-SYM (<lexeme>)
   rule choice(seq(nil(#f), nil(#f), lit-rename, colon, colon-sep),
               seq(pound, quote, lit-rename, quote, quote-sep))
   => tokens;
   slot value :: <string> = tokens[2];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-REQUIRED-INIT-KEYWORD-SYM (<lexeme>)
   rule choice(seq(nil(#f), nil(#f), lit-req-init-keyword, colon, colon-sep),
               seq(pound, quote, lit-req-init-keyword, quote, quote-sep))
   => tokens;
   slot value :: <string> = tokens[2];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-REQUIRED (<lexeme>)
   rule seq(lit-required, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-SEALED (<lexeme>)
   rule seq(lit-sealed, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-SETTER-SYM (<lexeme>)
   rule choice(seq(nil(#f), nil(#f), lit-setter, colon, colon-sep),
               seq(pound, quote, lit-setter, quote, quote-sep))
   => tokens;
   slot value :: <string> = tokens[2];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-SLOT (<lexeme>)
   rule seq(lit-slot, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-TYPE-SYM (<lexeme>)
   rule choice(seq(nil(#f), nil(#f), lit-type, colon, colon-sep),
               seq(pound, quote, lit-type, quote, quote-sep))
   => tokens;
   slot value :: <string> = tokens[2];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-USE (<lexeme>)
   rule seq(lit-use, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

//
// Reserved Words
//

define parser reserved-word :: <token>
   label "reserved word";
   rule choice(core-word, begin-word, function-word,
               define-body-word, define-list-word)
   => token;
   yield token;
end;

define parser core-word (<token>)
   rule seq(choice(lit-define, lit-end, lit-handler, lit-let, lit-local, lit-macro,
                   lit-otherwise),
            word-sep)
   => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-token-end(value, tokens);
end;

define caching parser begin-word (<token>)
   rule seq(choice(lit-begin, lit-block, lit-case, lit-for, lit-if, lit-method,
                   lit-select, lit-unless, lit-until, lit-while),
            word-sep)
   => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-token-end(value, tokens);
end;

define parser lex-BEGIN-WORD (<lexeme>)
   rule begin-word => token;
   slot value :: <string> = token.value;
afterwards (context, token, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   value.parse-end := token.parse-end;
end;

define caching parser function-word (<token>)
   rule seq(choice(), word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-token-end(value, tokens);
end;

define parser lex-FUNCTION-WORD (<lexeme>)
   rule function-word => token;
   slot value :: <string> = token.value;
afterwards (context, token, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   value.parse-end := token.parse-end;
end;

define caching parser define-body-word (<token>)
   rule seq(choice(lit-class, lit-library, lit-method, lit-function, lit-module),
            word-sep)
   => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-token-end(value, tokens);
end;

define parser lex-DEFINE-BODY-WORD (<lexeme>)
   rule define-body-word => token;
   slot value :: <string> = token.value;
afterwards (context, token, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   value.parse-end := token.parse-end;
end;

define caching parser define-list-word (<token>)
   rule seq(choice(lit-constant, lit-variable, lit-generic, lit-domain),
            word-sep)
   => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-token-end(value, tokens);
end;

define parser lex-DEFINE-LIST-WORD (<lexeme>)
   rule define-list-word => token;
   slot value :: <string> = token.value;
afterwards (context, token, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   value.parse-end := token.parse-end;
end;

//
// Reserved words used in definitions.
//

define parser lex-CLASS (<lexeme>)
   rule seq(lit-class, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-CONSTANT (<lexeme>)
   rule seq(lit-constant, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-DOMAIN (<lexeme>)
   rule seq(lit-domain, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-FUNCTION (<lexeme>)
   rule seq(lit-function, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-GENERIC (<lexeme>)
   rule seq(lit-generic, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-LIBRARY (<lexeme>)
   rule seq(lit-library, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-MODULE (<lexeme>)
   rule seq(lit-module, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-VARIABLE (<lexeme>)
   rule seq(lit-variable, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
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
   rule seq(name, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser unreserved-name :: <string>
   rule seq(not-next(reserved-word), name) => tokens;
   yield tokens[1];
end;

define parser lex-UNRESERVED-NAME (<lexeme>)
   label "name";
   rule seq(unreserved-name, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser ordinary-name :: <string>
   rule choice(seq(nil(identity), unreserved-name),
               seq(nil(value), define-body-word),
               seq(nil(value), define-list-word))
   => tokens;
   yield (tokens[0])(tokens[1]);
end;

define parser lex-ORDINARY-NAME (<lexeme>)
   label "name";
   rule seq(ordinary-name, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-CONSTRAINED-NAME (<lexeme>)
   label "constrained name";
   rule choice(seq(name, colon, word, word-sep),
               seq(name, colon, binary-operator-with-sep),
               seq(colon, word, word-sep))
   => tokens;
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser operator-name :: <string>
   rule seq(backslash, choice(unary-operator, binary-operator)) => tokens;
   yield tokens.concatenated-strings;
end;

define parser macro-name :: <string>
   rule choice(seq(nil(identity), ordinary-name),
               seq(nil(value), begin-word),
               seq(nil(value), function-word))
   => tokens;
   yield (tokens[0])(tokens[1]);
end;

define parser lex-MACRO-NAME (<lexeme>)
   label "name";
   rule seq(macro-name, word-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-NAME-NOT-END (<lexeme>)
   label "name";
   rule seq(choice(macro-name, lit-define, lit-handler, lit-let,
                   lit-local, lit-macro, lit-otherwise),
            word-sep)
   => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define parser lex-SYMBOL (<lexeme>)
   label "symbol";
   rule choice(seq(nil(#f), word, colon, colon-sep),
               seq(pound, string, quote-sep))
   => tokens;
   slot value :: <string> = tokens[1];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
end;

define caching parser word :: <string>
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

define parser unary-operator-with-sep (<token>)
   rule choice(seq(minus, minus-sep),
               seq(not, not-sep))
   => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-token-end(value, tokens);
end;

define parser lex-UNARY-OPERATOR (<lexeme>)
   label "unary operator";
   rule unary-operator-with-sep => token;
   slot value :: <string> = token.value;
afterwards (context, token, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   value.parse-end := token.parse-end;
end;

define parser binary-operator :: <string>
   label "binary operator";
   rule choice(identical, not-identical, not-equal, lt-equal, gt-equal, bind,
               plus, minus, star, slash, caret, equal, lt, gt, amp, vert-bar)
   => token;
   yield token;
end;

define parser binary-operator-with-sep (<token>)
   rule choice(seq(identical,       identical-sep     ),
               seq(not-identical,   not-identical-sep ),
               seq(not-equal,       not-equal-sep     ),
               seq(lt-equal,        lt-equal-sep      ),
               seq(gt-equal,        gt-equal-sep      ),
               seq(bind,            bind-sep          ),
               seq(plus,            plus-sep          ),
               seq(minus,           minus-sep         ),
               seq(star,            star-sep          ),
               seq(slash,           slash-sep         ),
               seq(caret,           caret-sep         ),
               seq(equal,           equal-sep         ),
               seq(lt,              lt-sep            ),
               seq(gt,              gt-sep            ),
               seq(amp,             amp-sep           ),
               seq(vert-bar,        vert-bar-sep      ))
   => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   adjust-token-end(value, tokens);
end;

define parser lex-BINARY-OPERATOR (<lexeme>)
   label "binary operator";
   rule binary-operator-with-sep => token;
   slot value :: <string> = token.value;
afterwards (context, token, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   value.parse-end := token.parse-end;
end;

//
// Character and String Literals
//

define parser lex-CHARACTER-LITERAL (<lexeme>)
   label "character literal";
   rule seq(apos, character, apos, apos-sep) => tokens;
   slot value :: <string> = tokens[1];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
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
   rule seq(string, quote-sep) => tokens;
   slot value :: <string> = tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
   adjust-token-end(value, tokens);
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
   rule choice(integer, ratio, floating-point) => token;
   slot value = token;
afterwards (context, tokens, value, start-pos, end-pos)
   consume-last-whitespace-doc(value, context);
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

module: template-engine


define class <dirspec-token> (<token>)
   slot dirspec-lexeme :: false-or(<lexeme>), init-keyword: #"lexeme"
end class;

define class <test-dirspec-token> (<dirspec-token>)
   slot test-expression :: <expression-token>
end class;

define class <syntax-token> (<token>)
end class;

define class <expression-token> (<syntax-token>)
   slot const-value? :: <boolean> = #f;
   slot value :: <object> = #f;
end class;


define parser case-dirspec (<test-dirspec-token>)
   rule seq(lex-case, templ-space, expression, eof) => tokens;
   inherited slot dirspec-lexeme = tokens[0];
   inherited slot test-expression = tokens[2];
end;


define parser if-dirspec (<test-dirspec-token>)
   rule seq(choice(lex-if, lex-when, lex-unless), templ-space, expression, eof)
      => tokens;
   inherited slot dirspec-lexeme = tokens[0];
   inherited slot test-expression = tokens[2];
end;


define parser repeat-dirspec (<dirspec-token>)
   rule seq(lex-repeat, templ-space, lex-with, templ-space, lex-name, templ-space,
            lex-in, templ-space, expression,
            opt-seq(templ-space, lex-info, templ-space, lex-in, templ-space, lex-name),
            eof)
      => tokens;
   inherited slot dirspec-lexeme = tokens[0];
   slot loop-item-name :: <string> = tokens[4].text;
   slot loop-info-name :: false-or(<string>) = tokens[9] & (tokens[9][5].text);
   slot collection-expression :: <expression-token> = tokens[8];
end;


define parser with-dirspec (<dirspec-token>)
   rule seq(lex-with, templ-space, assignment,
            opt-many(seq(opt-templ-space, lex-comma, opt-templ-space, assignment)),
            eof)
      => tokens;
   inherited slot dirspec-lexeme = tokens[0];
   slot assignments :: <sequence>
      = concatenate(vector(tokens[2]), collect-subelements(tokens[3], 3));
end;


define parser assignment (<syntax-token>)
   rule choice(seq(lex-operation, templ-space, lex-name, templ-space, lex-equal,
                   templ-space, lex-name),
               seq(nil(#f), nil(#f), lex-name, templ-space, lex-equal,
                   templ-space, expression))
      => tokens;
   slot operation? :: <boolean> = tokens[0].true?;
   slot name :: <string> = tokens[2].text;
   slot value-token :: type-union(<lex-name-token>, <expression-token>)
      = tokens.last;
end;


define parser simple-dirspec (<dirspec-token>)
   rule seq(expression, eof) => tokens;
   inherited slot dirspec-lexeme = #f;
   slot expression :: <expression-token> = tokens[0];
end;


define parser else-dirspec (<dirspec-token>)
   rule seq(lex-else, eof) => tokens;
   inherited slot dirspec-lexeme = tokens[0];
end;


define parser end-dirspec (<dirspec-token>)
   rule seq(lex-end, eof) => tokens;
   inherited slot dirspec-lexeme = tokens[0];
end;


//
// Expressions
//


define class <binary-expression-token> (<expression-token>)
   slot left-operand :: <expression-token>;
   slot operator :: <lexeme>;
   slot right-operand :: <expression-token>;
end class;


define parser expression :: <expression-token>
   rule tier-1-or-lower-expression => token;
   yield token
end;


define parser tier-1-or-lower-expression :: <expression-token>
   rule choice(binary-tier-1-expression, tier-2-or-lower-expression) => token;
   yield token
end;

define parser tier-2-or-lower-expression :: <expression-token>
   rule choice(binary-tier-2-expression, tier-3-or-lower-expression) => token;
   yield token
end;

define parser tier-3-or-lower-expression :: <expression-token>
   rule choice(binary-tier-3-expression, tier-4-or-lower-expression) => token;
   yield token
end;

define parser tier-4-or-lower-expression :: <expression-token>
   rule choice(binary-tier-4-expression, binary-operand) => token;
   yield token
end;


define parser binary-tier-1-expression (<binary-expression-token>)
   rule seq(tier-2-or-lower-expression, opt-templ-space, binary-tier-1-operator,
            opt-templ-space, expression)
      => tokens;
   inherited slot left-operand = tokens[0];
   inherited slot operator = tokens[2];
   inherited slot right-operand = tokens[4];
afterwards (context :: <template-context>, tokens, result, start-pos, end-pos)
   resolve-const-expression(context, result)
end;


define caching parser binary-tier-2-expression (<binary-expression-token>)
   rule seq(tier-3-or-lower-expression, opt-templ-space, binary-tier-2-operator,
            opt-templ-space, expression)
      => tokens;
   inherited slot left-operand = tokens[0];
   inherited slot operator = tokens[2];
   inherited slot right-operand = tokens[4];
afterwards (context :: <template-context>, tokens, result, start-pos, end-pos)
   resolve-const-expression(context, result)
end;


define caching parser binary-tier-3-expression (<binary-expression-token>)
   rule seq(tier-4-or-lower-expression, opt-templ-space, binary-tier-3-operator,
            opt-templ-space, expression)
      => tokens;
   inherited slot left-operand = tokens[0];
   inherited slot operator = tokens[2];
   inherited slot right-operand = tokens[4];
afterwards (context :: <template-context>, tokens, result, start-pos, end-pos)
   resolve-const-expression(context, result)
end;


define caching parser binary-tier-4-expression (<binary-expression-token>)
   rule seq(binary-operand, opt-templ-space, binary-tier-4-operator,
            opt-templ-space, expression)
      => tokens;
   inherited slot left-operand = tokens[0];
   inherited slot operator = tokens[2];
   inherited slot right-operand = tokens[4];
afterwards (context :: <template-context>, tokens, result, start-pos, end-pos)
   resolve-const-expression(context, result)
end;


define caching parser binary-operand (<expression-token>)
   rule seq(opt(unary-operator), opt-templ-space, operand) => tokens;
   slot operator :: false-or(<lexeme>) = tokens[0];
   slot operand :: <expression-token> = tokens[2];
afterwards (context :: <template-context>, tokens, result, start-pos, end-pos)
   resolve-const-expression(context, result)
end;


define parser binary-tier-1-operator :: <lexeme>
   rule choice(lex-ampersand, lex-vert-bar) => token;
   yield token
end;

define parser binary-tier-2-operator :: <lexeme>
   rule choice(lex-star, lex-slash, lex-percent) => token;
   yield token
end;

define parser binary-tier-3-operator :: <lexeme>
   rule choice(lex-plus, lex-minus) => token;
   yield token
end;

define parser binary-tier-4-operator :: <lexeme>
   rule choice(lex-equal, lex-not-equal, lex-lf-angle, lex-rt-angle, lex-lte, lex-gte)
      => token;
   yield token
end;


define parser unary-operator :: <lexeme>
   rule choice(lex-minus, lex-not) => token;
   yield token
end;


define parser operand (<expression-token>)
   rule seq(choice(paren-expression, var-name, literal),
            opt-many(seq(opt-templ-space, lex-period, opt-templ-space, chained-call)))
      => tokens;
   slot base-value :: <expression-token> = tokens[0];
   slot chained-calls :: <sequence> = collect-subelements(tokens[1], 3);
afterwards (context :: <template-context>, tokens, result, start-pos, end-pos)
   resolve-const-expression(context, result)
end;


define parser paren-expression :: <expression-token>
   rule seq(lex-lf-paren, opt-templ-space, expression, opt-templ-space, lex-rt-paren)
      => tokens;
   yield tokens[2]
end;


define parser var-name (<expression-token>)
   rule lex-name => token;
   inherited slot value = #f;
   inherited slot const-value? = #f;
   slot name :: <string> = token.text;
end;


define parser chained-call (<syntax-token>)
   rule lex-name => token;
   slot name :: <string> = token.text;
end;


//
// Expression literals
//


define parser literal :: <expression-token>
   rule choice(string, character, lex-number, lex-hex-number, lex-true, lex-false)
      => token;
   yield token
end;


define parser string (<expression-token>)
   rule seq(substring, opt-many(seq(opt-templ-space, substring))) => tokens;
   inherited slot value
      = combine-substrings(tokens[0], collect-subelements(tokens[1], 1));
   inherited slot const-value? = #t;
end;


define method combine-substrings (base :: <substring-token>, rest :: <sequence>)
=> (string :: <string>)
   apply(concatenate, base.value, map(value, rest))
end method;


define parser substring (<expression-token>)
   rule seq(lit-dquote, opt-many(seq(not-next(lit-dquote), string-char)), lit-dquote)
      => tokens;
   inherited slot value
         = (tokens[1] & as(<string>, collect-subelements(tokens[1], 1))) | "";
   inherited slot const-value? = #t;
end;


define parser character (<expression-token>)
   rule seq(lit-quote, string-char, lit-quote) => tokens;
   inherited slot value = tokens[1];
   inherited slot const-value? = #t;
end;


define parser string-char :: <character>
   rule choice(code-escape, char-escape, char) => token;
   yield token
end;


define parser code-escape :: <character>
   rule seq(lit-backslash, lit-lf-angle, many(hex-digit), lit-rt-angle) => tokens;
   yield as(<character>, string-to-integer(as(<string>, tokens[2]), base: 16))
end;


define parser char-escape :: <character>
   rule seq(lit-backslash, esc-char) => tokens;
   yield character-for-escape-char(tokens.last)
end;


define method character-for-escape-char (char :: <character>) => (char :: <character>)
   select (char)
      '\''     => '\'';
      '\"'     => '\"';
      'a', 'A' => '\<07>';
      'b', 'B' => '\<08>';
      'e', 'E' => '\<1B>';
      'f', 'F' => '\<0C>';
      'n', 'N' => '\<0A>';
      'r', 'R' => '\<0D>';
      't', 'T' => '\<09>';
      '0'      => '\<00>';
   end select;
end method;

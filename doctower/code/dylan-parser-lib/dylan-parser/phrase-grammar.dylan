module: dylan-parser
synopsis: The Dylan phrase grammar, as specified in the DRM, with some omissions.

define class <text-token> (<source-location-token>)
end class;

//
// Program Structure
//

define parser body (<token>)
   rule seq(constituents, opt(lex-SEMICOLON)) => tokens;
end;

define parser constituents (<token>)
   rule seq(constituent, opt-many(seq(lex-SEMICOLON, constituent))) => tokens;
end;

define parser constituent (<token>)
   rule choice( /* (disallowed in bodies) definition, */ local-declaration, expression)
   => token;
end;

/* (unused)
define parser \macro ()
   rule choice(definition-macro-call, statement, function-macro-call) => token;
end;
*/

//
// Property Lists
//

define parser comma-property-list :: <sequence> /* of <property-token> */
   rule seq(lex-COMMA, property-list) => tokens;
   yield tokens[1];
end;

define parser property-list :: <sequence> /* of <property-token> */
   rule seq(property, opt-many(seq(lex-COMMA, property))) => tokens;
   yield list-from-tokens(tokens);
end;

define parser property (<source-location-token>)
   rule seq(lex-SYMBOL, value) => tokens;
   slot prop-name :: <string> = tokens[0].value;
   slot prop-value :: <text-token> = tokens[1];
afterwards (context, tokens, value, start-pos, end-pos)
   note-combined-source-location(context, value, tokens);
end;

define parser value :: <text-token>
   rule basic-fragment => token;
   yield token;
end;

//
// Fragments
//

define parser body-fragment (<token>)
   rule choice(seq(statement, opt(non-statement-body-fragment)),
               non-statement-body-fragment)
   => tokens;
end;

define parser list-fragment (<token>)
   rule choice(seq(statement, opt(non-statement-list-fragment)),
               non-statement-list-fragment)
   => tokens;
end;

define parser basic-fragment (<text-token>)
   rule choice(seq(statement, opt(non-statement-basic-fragment)),
               non-statement-basic-fragment)
   => token;
afterwards (context, tokens, value, start-pos, end-pos)
   note-combined-source-location(context, value, tokens);
end;

define parser non-statement-body-fragment (<token>)
   rule choice(seq(definition, opt(semicolon-fragment)),
               seq(local-declaration, opt(semicolon-fragment)),
               seq(simple-fragment, opt(body-fragment)),
               seq(lex-COMMA, opt(body-fragment)),
               semicolon-fragment)
   => tokens;
end;

define parser semicolon-fragment (<token>)
   rule seq(lex-SEMICOLON, opt(body-fragment)) => tokens;
end;

define parser non-statement-list-fragment (<token>)
   rule seq(choice(simple-fragment, lex-COMMA), opt(list-fragment)) => tokens;
end;

define parser non-statement-basic-fragment (<token>)
   rule seq(simple-fragment, opt(basic-fragment)) => tokens;
end;

define parser simple-fragment (<token>)
   rule choice(function-macro-call, constant-fragment, variable-name,
               lex-BINARY-OPERATOR, lex-UNARY-OPERATOR, bracketed-fragment, 
               lex-POUND-WORD, lex-PERIOD, lex-DOUBLE-COLON, lex-ARROW,
               lex-DOUBLE-QUESTION, lex-QUESTION-EQUAL, lex-QUESTION,
               lex-ELLIPSIS, lex-DOUBLE-POUND, lex-OTHERWISE)
   => token;
end;

define parser bracketed-fragment (<token>)
   rule choice(seq(lex-LF-PAREN, opt(body-fragment), lex-RT-PAREN),
               seq(lex-LF-BRACK, opt(body-fragment), lex-RT-BRACK),
               seq(lex-LF-BRACE, opt(body-fragment), lex-RT-BRACE))
   => tokens;
end;

define parser constant-fragment (<token>)
   rule choice(lex-NUMBER, lex-CHARACTER-LITERAL, lex-STRING, lex-SYMBOL,
               seq(lex-LF-LIST, constants, lex-PERIOD, constant, lex-RT-PAREN),
               seq(lex-LF-LIST, opt(constants), lex-RT-PAREN),
               seq(lex-LF-VECTOR, opt(constants), lex-RT-BRACK))
   => tokens;
end;

/* (replaced by definitions.dylan)
//
// Definitions
//

define parser definition ()
   rule choice(definition-macro-call,
               seq(lex-DEFINE, lex-MACRO, macro-definition))
   => token;
end parser;

define parser definition-macro-call ()
   rule seq(lex-DEFINE, opt-many(modifier),
            choice(seq(lex-DEFINE-BODY-WORD, opt(body-fragment), definition-tail),
                   seq(lex-DEFINE-LIST-WORD, opt(list-fragment))))
   => tokens;
end parser;

define parser modifier ()
   rule lex-UNRESERVED-NAME => tokens;
end parser;

define parser definition-tail ()
   rule choice(seq(lex-END, lex-DEFINE-BODY-WORD, lex-MACRO-NAME),
               seq(lex-END, opt(lex-MACRO-NAME)))
   => tokens;
end parser;
*/

//
// Local Declarations
//

define parser local-declaration (<token>)
   rule choice(seq(lex-LET, bindings),
               seq(lex-LET, lex-HANDLER, condition, lex-EQUAL, \handler),
               seq(lex-LOCAL, local-methods))
   => tokens;
end;

define parser condition (<token>)
   rule choice(type, seq(lex-LF-PAREN, type, comma-property-list, lex-RT-PAREN))
   => tokens;
end;

define parser \handler (<token>)
   rule expression => token;
end;

define parser local-methods (<token>)
   rule seq(opt(lex-METHOD),
            method-definition,
            opt-many(seq(lex-COMMA, method-definition)))
   => tokens;
end;

define parser bindings (<token>)
   rule seq(choice(variable, seq(lex-LF-PAREN, variable-list, lex-RT-PAREN)),
            lex-EQUAL, expression)
   => tokens;
end;

define parser variable-list (<token>)
   rule choice(seq(variables, opt-seq(lex-COMMA, lex-REST, variable-name)),
               seq(lex-REST, variable-name))
   => tokens;
end;

define parser variables :: <sequence> /* of <variable-token> */
   rule seq(variable, opt-many(seq(lex-COMMA, variable))) => tokens;
   yield list-from-tokens(tokens);
end;

define parser variable (<token>)
   rule seq(variable-name, opt-seq(lex-DOUBLE-COLON, checked-type)) => tokens;
   slot name :: <string> = tokens[0].name;
   slot type :: false-or(<text-token>) =
      tokens[1] & ~skipped?(tokens[1][1]) & tokens[1][1];
   slot var-doc :: false-or(<doc-comment-token>) = tokens[0].var-doc;
end;

define parser variable-name (<token>)
   rule lex-ORDINARY-NAME => token;
   slot name :: <string> = token.value;
   slot var-doc :: false-or(<doc-comment-token>) = token.lexeme-doc;
end;

define parser type :: <operand-token>
   rule operand => token;
   yield token;
end;

//
// Expressions
//

define parser expressions (<token>)
   label "expression";
   rule seq(expression, opt-many(seq(lex-COMMA, expression))) => tokens;
end;

define parser expression (<text-token>)
   rule seq(binary-operand, opt-many(seq(lex-BINARY-OPERATOR, binary-operand))) => tokens;
afterwards (context, tokens, value, start-pos, end-pos)
   note-combined-source-location(context, value, tokens);
end;

define parser expression-no-symbol (<token>)
   rule seq(binary-operand-no-symbol, opt-seq(lex-BINARY-OPERATOR, expression))
   => tokens;
end;

define parser binary-operand-no-symbol (<token>)
   rule seq(opt(lex-UNARY-OPERATOR), operand) => tokens;
end;

define parser binary-operand (<token>)
   rule choice(lex-SYMBOL, seq(opt(lex-UNARY-OPERATOR), operand)) => tokens;
end;

define parser operand (<text-token>)
   rule seq(leaf,
            opt-many(choice(seq(lex-LF-PAREN, opt(arguments), lex-RT-PAREN),
                            seq(lex-LF-BRACK, opt(arguments), lex-RT-BRACK),
                            seq(lex-PERIOD, variable-name))))
   => tokens;
afterwards (context, tokens, value, start-pos, end-pos)
   note-combined-source-location(context, value, tokens);
end;

define parser function-macro-call (<token>)
   rule seq(lex-FUNCTION-WORD, lex-LF-PAREN, opt(body-fragment), lex-RT-PAREN)
   => tokens;
end;

define parser leaf (<token>)
   rule choice(literal,
               statement,
               function-macro-call,
               variable-name,
               seq(lex-LF-PAREN, expression, lex-RT-PAREN))
   => tokens;
end;

define parser arguments (<token>)
   rule seq(argument, opt-many(seq(lex-COMMA, argument))) => tokens;
end;

define parser argument (<token>)
   rule choice(seq(lex-SYMBOL, opt(expression)),
               expression-no-symbol)
   => tokens;
end;

define parser literal (<token>)
   rule choice(lex-EMPTY-LIST, lex-NUMBER, lex-CHARACTER-LITERAL, string-literal,
               lex-TRUE, lex-FALSE, 
               seq(lex-LF-LIST, constants, lex-PERIOD, constant, lex-RT-PAREN),
               seq(lex-LF-LIST, opt(constants), lex-RT-PAREN),
               seq(lex-LF-VECTOR, opt(constants), lex-RT-BRACK))
   => tokens;
end;

define parser string-literal (<text-token>)
   rule many(lex-STRING) => tokens;
   slot value :: <string> = apply(concatenate, map(value, tokens));
afterwards (context, tokens, value, start-pos, end-pos)
   note-combined-source-location(context, value, tokens);
end;

define parser constants (<token>)
   rule seq(constant, opt-many(seq(lex-COMMA, constant))) => tokens;
end;

define parser constant (<token>)
   rule choice(literal, lex-SYMBOL) => token;
end;

//
// Statements
//

define parser statement (<token>)
   label "statement";
   rule seq(lex-BEGIN-WORD, opt(body-fragment), end-clause) => tokens;
end;

define parser end-clause (<token>)
   label "end clause";
   rule seq(lex-END, opt(lex-BEGIN-WORD)) => tokens;
end;

define parser case-body (<token>)
   rule seq(cases, opt(lex-SEMICOLON)) => tokens;
end;

define parser cases (<token>)
   rule seq(case-label,
            opt(constituents),
            opt-many(seq(lex-SEMICOLON, case-label, opt(constituents))))
   => tokens;
end;

define parser case-label (<token>)
   rule choice(seq(expressions, lex-ARROW),
               seq(lex-LF-PAREN, expression, lex-COMMA, expressions, lex-RT-PAREN,
                   lex-ARROW),
               seq(lex-OTHERWISE, opt(lex-ARROW)))
   => tokens;
end;

//
// Methods
//

define parser method-definition (<token>)
   rule seq(variable-name, parameter-list, opt(body),
            lex-END, opt(lex-METHOD), opt(variable-name))
   => tokens;
end;

define parser parameter-list (<token>)
   rule seq(lex-LF-PAREN, opt(parameters), lex-RT-PAREN,
            choice(seq(lex-ARROW, enclosed-values-list, opt(lex-SEMICOLON)),
                   seq(lex-ARROW, bare-values-list, lex-SEMICOLON),
                   opt-seq(lex-SEMICOLON, nil(#f))))
   => tokens;
   slot parameter-list = parameter-list-from-token(tokens[1]);
   slot value-list = value-list-from-token(tokens[3] & tokens[3][1]) | #[];
attributes
   // All parameter-list parts (except bare-values-list) have these followers and recovery.
   type-followers = vector(parse-lex-COMMA, parse-lex-RT-PAREN),
   type-skipper = parse-til-rt-paren,
   expression-followers = vector(parse-lex-COMMA, parse-lex-RT-PAREN),
   expression-skipper = parse-til-rt-paren
end;

define parser parameters (<token>)
   rule choice(seq(nil(#"no-req"), next-rest-key-parameter-list),
               seq(nil(#"req"), required-parameters,
                   opt-seq(lex-COMMA, next-rest-key-parameter-list)))
   => tokens;
   slot required-params :: <sequence> =
         select (tokens[0])
            #"no-req" => #[];
            #"req" => tokens[1];
         end select;
   slot rest-key-param :: false-or(<rest-key-parameter-list-token>) =
         select (tokens[0])
            #"no-req" => tokens[1];
            #"req" => tokens[2] & tokens[2][1];
         end select;
end;

// #next parameters aren't documented.
define parser next-rest-key-parameter-list :: <rest-key-parameter-list-token>
   rule choice(seq(lex-NEXT, variable-name,
                   opt-seq(lex-COMMA, rest-key-parameter-list)),
               seq(nil(#f), nil(#f),
                   seq(rest-key-parameter-list)))
   => tokens;
   yield (tokens[2] & tokens[2].last);
end;

define parser rest-key-parameter-list (<token>)
   rule choice(seq(nil(#"rest"), lex-REST, variable-name,
                   opt-seq(lex-COMMA, key-parameter-list)),
               seq(nil(#"key"), key-parameter-list))
   => tokens;
   slot rest-doc :: false-or(<doc-comment-token>) =
         select (tokens[0])
            #"rest" => tokens[1].lexeme-doc;
            #"key" => #f;
         end select;
   slot rest-var :: false-or(<variable-name-token>) =
         select (tokens[0])
            #"rest" => tokens[2];
            #"key" => #f;
         end select;
   slot key-param :: false-or(<key-parameter-list-token>) =
         select (tokens[0])
            #"rest" => tokens[3] & tokens[3][1];
            #"key" => tokens[1];
         end select;
afterwards (context, tokens, value, start-pos, end-pos)
   remove-from-outer-scope(context, value.rest-doc);
end;

define parser key-parameter-list (<token>)
   rule seq(lex-KEY, opt(keyword-parameters), opt-seq(lex-COMMA, lex-ALL-KEYS))
   => tokens;
   slot key-params :: <sequence> = tokens[1] | #[];
   slot all-keys? :: <boolean> = tokens[2].true?;
   slot all-keys-doc :: false-or(<doc-comment-token>) =
         tokens[2] & tokens[2][1].lexeme-doc;
afterwards (context, tokens, value, start-pos, end-pos)
   remove-from-outer-scope(context, value.all-keys-doc);
end;

define parser required-parameters :: <sequence> /* of <required-parameter-token> */
   rule seq(required-parameter, opt-many(seq(lex-COMMA, required-parameter)))
   => tokens;
   yield list-from-tokens(tokens);
end;

define parser required-parameter (<token>)
   rule choice(seq(nil(#t), variable-name, lex-IDENTICAL, checked-expression),
               seq(nil(#f), variable, nil(#f), nil(#f)))
   => tokens;
   slot req-sing? :: <boolean> = tokens[0];
   slot req-doc :: false-or(<doc-comment-token>) = tokens[1].var-doc;
   slot req-var :: type-union(<variable-name-token>, <variable-token>) = tokens[1];
   slot req-inst :: false-or(<text-token>) = ~skipped?(tokens[3]) & tokens[3];
afterwards (context, tokens, value, start-pos, end-pos)
   remove-from-outer-scope(context, value.req-doc);
end;

define parser keyword-parameters :: <sequence> /* of <keyword-parameter-token> */
   rule seq(keyword-parameter, opt-many(seq(lex-COMMA, keyword-parameter)))
   => tokens;
   yield list-from-tokens(tokens);
end;

define parser keyword-parameter (<token>)
   rule seq(opt(lex-SYMBOL), variable, opt(default)) => tokens;
   slot key-doc :: false-or(<doc-comment-token>) =
         (tokens[0] & tokens[0].lexeme-doc) | tokens[1].var-doc;
   slot key-symbol :: false-or(<string>) = tokens[0] & tokens[0].value;
   slot key-var :: <variable-token> = tokens[1];
   slot key-default :: false-or(<text-token>) = tokens[2];
attributes
   type-followers = add(attr(type-followers), parse-lex-EQUAL);
afterwards (context, tokens, value, start-pos, end-pos)
   remove-from-outer-scope(context, value.key-doc);
end;

define parser default :: false-or(<text-token>)
   rule seq(lex-EQUAL, checked-expression) => tokens;
   yield ~skipped?(tokens[1]) & tokens[1];
end;

define parser bare-values-list :: <variable-token>
   rule variable => token;
   yield token;
attributes
   type-followers = vector(parse-lex-SEMICOLON, parse-lex-EOF),
   type-skipper = parse-til-parsable
end;

define parser enclosed-values-list :: false-or(<values-list-token>)
   rule seq(lex-LF-PAREN, opt(values-list), lex-RT-PAREN) => tokens;
   yield tokens[1];
end;

define parser values-list (<token>)
   rule choice(seq(nil(#"vars"), variables, opt-seq(lex-COMMA, lex-REST, variable)),
               seq(nil(#"rest"), lex-REST, variable))
   => tokens;
   slot required-vals :: <sequence> /* of <variable-token> */ =
         select (tokens[0])
            #"vars" => tokens[1];
            #"rest" => #[];
         end select;
   slot rest-val :: false-or(<variable-token>) =
         select (tokens[0])
            #"vars" => tokens[2] & tokens[2][2];
            #"rest" => tokens[2];
         end select;
   slot rest-doc :: false-or(<doc-comment-token>) =
         select (tokens[0])
            #"vars" => tokens[2] & tokens[2][1].lexeme-doc;
            #"rest" => #f;
         end select;
afterwards (context, tokens, value, start-pos, end-pos)
   apply(remove-from-outer-scope, context, value.rest-doc,
         map(var-doc, value.required-vals));
end;

//
// Macro Definitions
//

define parser macro-definition (<token>)
   rule seq(lex-MACRO-NAME, main-rule-set, opt(aux-rule-sets),
            lex-END, opt(lex-MACRO), opt-seq(lex-MACRO-NAME))
   => tokens;
   slot name :: <string> = tokens[0].value;
   slot main-rule-set :: <sequence> = tokens[1];
end;

define parser main-rule-set :: <sequence> /* of main rules */
   rule choice(many(body-style-definition-rule),
               many(list-style-definition-rule),
               many(statement-rule),
               many(function-rule))
   => tokens;
   yield tokens;
end;

define parser body-style-definition-rule (<source-location-token>)
   rule seq(lex-LF-BRACE, lex-DEFINE, opt(definition-head), lex-MACRO-NAME,
            opt(pattern), opt(lex-SEMICOLON), lex-END, lex-RT-BRACE,
            lex-ARROW, rhs)
   => tokens;
afterwards (context, tokens, value, start-pos, end-pos)
   note-combined-source-location(context, value, copy-sequence(tokens, end: 8));
end;

define parser list-style-definition-rule (<source-location-token>)
   rule seq(lex-LF-BRACE, lex-DEFINE, opt(definition-head), lex-MACRO-NAME,
            opt(pattern), lex-RT-BRACE, lex-ARROW, rhs)
   => tokens;
afterwards (context, tokens, value, start-pos, end-pos)
   note-combined-source-location(context, value, copy-sequence(tokens, end: 6));
end;

define parser rhs (<token>)
   rule seq(lex-LF-BRACE, opt(template), lex-RT-BRACE, opt(lex-SEMICOLON)) => tokens;
end;

define parser definition-head (<token>)
  rule many(choice(modifier, pattern-variable)) => tokens;
end;

define parser statement-rule (<source-location-token>)
   rule seq(lex-LF-BRACE, lex-MACRO-NAME, opt(pattern), opt(lex-SEMICOLON),
            lex-END, lex-RT-BRACE, lex-ARROW, rhs)
   => tokens;
afterwards (context, tokens, value, start-pos, end-pos)
   note-combined-source-location(context, value, copy-sequence(tokens, end: 6));
end;

define parser function-rule (<source-location-token>)
   rule seq(lex-LF-BRACE, lex-MACRO-NAME, lex-LF-PAREN, opt(pattern), lex-RT-PAREN,
            lex-RT-BRACE, lex-ARROW, rhs)
   => tokens;
afterwards (context, tokens, value, start-pos, end-pos)
   note-combined-source-location(context, value, copy-sequence(tokens, end: 6));
end;

//
// Patterns
//

define parser pattern (<token>)
   rule seq(pattern-list, opt-many(seq(lex-SEMICOLON, pattern-list))) => tokens;
end;

define parser pattern-list (<token>)
   rule choice(property-list-pattern,
               seq(pattern-sequence, opt-seq(lex-COMMA, pattern-list))) => token;
end;

define parser pattern-sequence (<token>)
   rule many(simple-pattern) => tokens;
end;

define parser simple-pattern (<token>)
   rule choice(binding-pattern, pattern-variable, lex-NAME-NOT-END, lex-ARROW,
               bracketed-pattern)
   => token;
end;

define parser bracketed-pattern (<token>)
   rule choice(seq(lex-LF-PAREN, opt(pattern), lex-RT-PAREN),
               seq(lex-LF-BRACK, opt(pattern), lex-RT-BRACK),
               seq(lex-LF-BRACE, opt(pattern), lex-RT-BRACE))
   => tokens;
end;

define parser binding-pattern (<token>)
   rule choice(seq(pattern-variable, lex-DOUBLE-COLON, pattern-variable,
                   opt-seq(lex-EQUAL, pattern-variable)),
               seq(pattern-variable, lex-EQUAL, pattern-variable))
   => tokens;
end;

define parser pattern-variable (<token>)
   rule choice(seq(lex-QUESTION, lex-CONSTRAINED-NAME),
               seq(lex-QUESTION, lex-NAME),
               lex-ELLIPSIS)
   => token;
end;

define parser property-list-pattern (<token>)
   rule choice(seq(lex-REST, pattern-variable,
                   opt-seq(lex-COMMA, lex-KEY, opt(pattern-keywords))),
               seq(lex-KEY, opt(pattern-keywords)))
   => tokens;
end;

define parser pattern-keywords (<token>)
   rule choice(lex-ALL-KEYS,
               seq(pattern-keyword, opt-seq(lex-COMMA, pattern-keywords)))
   => token;
end;

define parser pattern-keyword (<token>)
   rule seq(choice(lex-QUESTION, lex-DOUBLE-QUESTION),
            choice(lex-NAME, lex-CONSTRAINED-NAME),
            opt(default))
   => tokens;
end;

//
// Templates
//

define parser template (<token>)
   rule many(template-element) => tokens;
end;

define parser template-element (<token>)
   rule choice(substitution, lex-PERIOD, lex-DOUBLE-COLON, lex-ARROW, lex-SYMBOL,
               lex-NAME, lex-NUMBER, lex-CHARACTER-LITERAL, lex-STRING, separator,
               lex-UNARY-OPERATOR, lex-POUND-WORD,
               seq(lex-LF-PAREN, opt(template), lex-RT-PAREN),
               seq(lex-LF-BRACK, opt(template), lex-RT-BRACK),
               seq(lex-LF-BRACE, opt(template), lex-RT-BRACE),
               seq(lex-LF-LIST, opt(template), lex-RT-PAREN),
               seq(lex-LF-VECTOR, opt(template), lex-RT-BRACK))
   => token;
end;

define parser separator (<token>)
   rule choice(lex-SEMICOLON, lex-COMMA, lex-BINARY-OPERATOR) => token;
end;

define parser substitution (<token>)
   rule choice(seq(opt(name-prefix), lex-QUESTION, name-string-or-symbol, opt(name-suffix)),
               seq(lex-DOUBLE-QUESTION, lex-NAME, opt(separator), lex-ELLIPSIS),
               lex-ELLIPSIS,
               seq(lex-QUESTION-EQUAL, lex-NAME))
   => token;
end;

define parser name-prefix (<token>)
   rule seq(lex-STRING, lex-DOUBLE-POUND) => tokens;
end;

define parser name-suffix (<token>)
   rule seq(lex-DOUBLE-POUND, lex-STRING) => tokens;
end;

define parser name-string-or-symbol (<token>)
   rule choice(lex-NAME, lex-STRING, lex-SYMBOL) => tokens;
end;

//
// Auxiliary Rule Sets
//

define parser aux-rule-sets (<token>)
   rule many(aux-rule-set) => tokens;
end;

define parser aux-rule-set (<token>)
   rule seq(lex-SYMBOL, aux-rules) => tokens;
end;

define parser aux-rules (<token>)
   rule many(aux-rule) => tokens;
end;

define parser aux-rule (<token>)
   rule seq(lex-LF-BRACE, opt(pattern), lex-RT-BRACE, lex-ARROW, rhs) => tokens;
end;

module: dylan-parser
synopsis: Grammar of top-level definitions.

define class <definition-token> (<source-location-token>)
   slot scoped-docs :: <stretchy-vector> = make(<stretchy-vector>);
   slot api-name :: <string>;
end class;

//
// Definitions
//

define caching parser definition :: <definition-token>
   rule seq(lex-DEFINE,
            choice(variable-definer, constant-definer, class-definer,
                   generic-definer, method-definer, function-definer,
                   module-definer, library-definer, macro-definer, domain-definer))
   => tokens;
   yield tokens[1];
attributes
   // Default recovery methods.
   type-skipper = parse-til-parsable,
   expression-skipper = parse-til-parsable;
afterwards (context, tokens, value, start-pos, end-pos)
   value.scoped-docs := add-to-front(tokens[0].lexeme-doc, value.scoped-docs);
   remove-from-outer-scope(context, tokens[0].lexeme-doc);
   note-combined-source-location(context, value, tokens);
end;

define caching parser modifier :: <token>
   label "adjective";
   rule lex-UNRESERVED-NAME => token;
   yield token;
end;

//
// Variables & constants
//

define parser variable-definer (<definition-token>)
   // Multiple-variable form is unsupported. TODO: Add a warning if we see it.
   rule seq(opt-many(modifier), lex-VARIABLE, variable, lex-EQUAL, expression)
   => tokens;
   inherited slot scoped-docs = attr(scoped-docs);
   inherited slot api-name = tokens[2].name;
   slot api-modifiers = (tokens[0] & map(value, tokens[0])) | #[];
   slot api-type = tokens[2].type;
   slot api-value = tokens[4];
attributes
   scoped-docs = make(<stretchy-vector>),
   // Variable types and init expression have these followers.
   type-followers = vector(parse-lex-EQUAL),
   expression-followers = vector(parse-lex-SEMICOLON, parse-lex-EOF);
afterwards (context, tokens, value, start-pos, end-pos)
   note-combined-source-location(context, value, tokens);
end;

define parser constant-definer (<definition-token>)
   // Multiple-constant form is unsupported. TODO: Add a warning if we see it.
   rule seq(opt-many(modifier), lex-CONSTANT, variable, lex-EQUAL, expression)
   => tokens;
   inherited slot scoped-docs = attr(scoped-docs);
   inherited slot api-name = tokens[2].name;
   slot api-modifiers = (tokens[0] & map(value, tokens[0])) | #[];
   slot api-type = tokens[2].type;
   slot api-value = tokens[4];
attributes
   scoped-docs = make(<stretchy-vector>),
   // Variable types and init expression have these followers.
   type-followers = vector(parse-lex-EQUAL),
   expression-followers = vector(parse-lex-SEMICOLON, parse-lex-EOF);
afterwards (context, tokens, value, start-pos, end-pos)
   note-combined-source-location(context, value, tokens);
end;

//
// Classes
//

define parser class-definer (<definition-token>)
   rule seq(opt-many(modifier), lex-CLASS, variable-name,
            lex-LF-PAREN, superclasses, lex-RT-PAREN, opt(class-clauses))
   => tokens;
   inherited slot scoped-docs = attr(scoped-docs);
   inherited slot api-name = tokens[2].name;
   slot api-modifiers :: <sequence> = (tokens[0] & map(value, tokens[0])) | #[];
   slot class-supers :: <sequence> /* of <text-token> */ = tokens[4];
   slot class-slots =
      choose(true?, map(slot-from-clause, tokens[6] | #[]));
   slot class-keywords =
      remove-duplicate-keywords(choose(true?, map(keyword-from-clause, tokens[6] | #[])));
attributes
   scoped-docs = make(<stretchy-vector>),
   // Superclasses have these followers and recovery.
   expression-followers = vector(parse-lex-RT-PAREN, parse-lex-COMMA),
   expression-skipper = parse-til-rt-paren;
afterwards (context, tokens, value, start-pos, end-pos)
   note-combined-source-location(context, value, tokens);
end;

define parser superclasses :: <sequence> /* of <text-token> */
   rule seq(checked-expression, opt-many(seq(lex-COMMA, checked-expression)),
            opt(lex-COMMA))
   => tokens;
   yield choose-unskipped(list-from-tokens(tokens));
end;

define parser class-clauses :: <sequence>
   rule seq(class-clause, opt-many(seq(lex-SEMICOLON, class-clause)),
            opt(lex-SEMICOLON))
   => tokens;
   yield list-from-tokens(tokens);
attributes
   // All clause options and slot definitions have these followers and recovery.
   expression-followers = vector(parse-lex-COMMA, parse-lex-SEMICOLON, parse-lex-END),
   expression-skipper = parse-til-class-clause,
   type-followers = vector(parse-lex-EQUAL, parse-lex-COMMA, parse-lex-SEMICOLON, parse-lex-END),
   type-skipper = parse-til-class-clause
end;

define parser class-clause :: <token>
   rule choice(init-arg-spec, inherited-slot-spec, slot-spec) => token;
   yield token;
end;

define parser init-arg-spec (<token>)
   rule seq(opt(lex-REQUIRED), lex-KEYWORD, lex-SYMBOL, opt(init-expression),
            opt-many(seq(lex-COMMA, init-arg-option)), opt(lex-COMMA))
   => tokens;
   slot clause-doc = (tokens[0] & tokens[0].lexeme-doc) | tokens[1].lexeme-doc;
   slot keyword-required? :: <boolean> = tokens[0].true?;
   slot keyword-name :: <string> = tokens[2].value;
   slot init-expression :: false-or(<text-token>) = tokens[3];
   slot clause-options = list-from-tokens(vector(#f, tokens[4], tokens[5]));
afterwards (context, tokens, value, start-pos, end-pos)
   remove-from-outer-scope(context, value.clause-doc)
end;

define parser inherited-slot-spec (<token>)
   rule seq(lex-INHERITED, lex-SLOT, variable-name, opt(init-expression),
            opt-many(seq(lex-COMMA, inherited-option)), opt(lex-COMMA))
   => tokens;
   slot clause-doc = tokens[0].lexeme-doc;
   slot slot-name :: <string> = tokens[2].name;
   slot init-expression :: false-or(<text-token>) = tokens[3];
   slot clause-options = list-from-tokens(vector(#f, tokens[4], tokens[5]));
afterwards (context, tokens, value, start-pos, end-pos)
   remove-from-outer-scope(context, value.clause-doc)
end;

define parser slot-spec (<token>)
   rule seq(opt(slot-adjectives), lex-SLOT, variable, opt(init-expression),
            opt-many(seq(lex-COMMA, slot-option)), opt(lex-COMMA))
   => tokens;
   slot clause-doc = (tokens[0] & tokens[0].clause-doc) | tokens[1].lexeme-doc;
   slot slot-modifiers = (tokens[0] & tokens[0].slot-modifiers) | #[];
   slot slot-name :: <string> = tokens[2].name;
   slot slot-type :: false-or(<text-token>) = tokens[2].type;
   slot init-expression :: false-or(<text-token>) = tokens[3];
   slot clause-options = list-from-tokens(vector(#f, tokens[4], tokens[5]));
afterwards (context, tokens, value, start-pos, end-pos)
   remove-from-outer-scope(context, value.clause-doc)
end;

define parser slot-adjectives (<token>)
   rule many(seq(not-next(lex-SLOT), lex-UNRESERVED-NAME)) => tokens;
   slot clause-doc = tokens.first.second.lexeme-doc;
   slot slot-modifiers = map(value, collect-subelements(tokens, 1));
end;

define parser init-expression :: false-or(<text-token>)
   rule seq(lex-EQUAL, checked-expression) => tokens;
   yield ~skipped?(tokens[1]) & tokens[1];
end;

define parser slot-option :: <token>
   rule choice(setter-option, init-keyword-option, required-init-keyword-option,
               type-option, init-value-option, init-function-option)
   => token;
   yield token;
end;

define parser init-arg-option :: <token>
   rule choice(type-option, init-value-option, init-function-option) => token;
   yield token;
end;

define parser inherited-option :: <token>
   rule choice(init-value-option, init-function-option) => token;
   yield token;
end;

define parser setter-option (<token>)
   rule seq(lex-SETTER-SYM, choice(variable-name, lex-FALSE)) => tokens;
   slot name :: false-or(<string>) =
         instance?(tokens[1], <variable-name-token>) & tokens[1].name;
end;

define parser init-keyword-option (<token>)
   rule seq(lex-INIT-KEYWORD-SYM, lex-SYMBOL) => tokens;
   slot value = tokens[1].value;
end;

define parser required-init-keyword-option (<token>)
   rule seq(lex-REQUIRED-INIT-KEYWORD-SYM, lex-SYMBOL) => tokens;
   slot value = tokens[1].value;
end;

define parser init-value-option (<token>)
   rule seq(lex-INIT-VALUE-SYM, checked-expression) => tokens;
   slot value = ~skipped?(tokens[1]) & tokens[1];
end;

define parser init-function-option (<token>)
   rule seq(lex-INIT-FUNCTION-SYM, checked-expression) => tokens;
   slot value = ~skipped?(tokens[1]) & tokens[1];
end;

define parser type-option (<token>)
   rule seq(lex-TYPE-SYM, checked-expression) => tokens;
   slot value = ~skipped?(tokens[1]) & tokens[1];
end;

//
// Generics & functions
//

define parser generic-definer (<definition-token>)
   rule seq(opt-many(modifier), lex-GENERIC, variable-name, 
            generic-parameter-list, opt(generic-options))
   => tokens;
   inherited slot scoped-docs = attr(scoped-docs);
   inherited slot api-name = tokens[2].name;
   slot api-modifiers = (tokens[0] & map(value, tokens[0])) | #[];
   slot func-params = tokens[3].parameter-list | #[];
   slot func-values = tokens[3].value-list | #[];
   slot func-options = tokens[4] | #[];
attributes
   scoped-docs = make(<stretchy-vector>);
afterwards (context, tokens, value, start-pos, end-pos)
   note-combined-source-location(context, value, tokens)
end;

define parser method-definer (<definition-token>)
   rule seq(opt-many(modifier), lex-METHOD, variable-name, parameter-list)
   => tokens;
   inherited slot scoped-docs = attr(scoped-docs);
   inherited slot api-name = tokens[2].name;
   slot api-modifiers = (tokens[0] & map(value, tokens[0])) | #[];
   slot func-params = tokens[3].parameter-list | #[];
   slot func-values = tokens[3].value-list | #[];
attributes
   scoped-docs = make(<stretchy-vector>);
afterwards (context, tokens, value, start-pos, end-pos)
   note-combined-source-location(context, value, tokens);
end;

define parser function-definer (<definition-token>)
   rule seq(opt-many(modifier), lex-FUNCTION, variable-name, parameter-list)
   => tokens;
   inherited slot scoped-docs = attr(scoped-docs);
   inherited slot api-name = tokens[2].name;
   slot api-modifiers = (tokens[0] & map(value, tokens[0])) | #[];
   slot func-params = tokens[3].parameter-list | #[];
   slot func-values = tokens[3].value-list | #[];
attributes
   scoped-docs = make(<stretchy-vector>);
afterwards (context, tokens, value, start-pos, end-pos)
   note-combined-source-location(context, value, tokens);
end;

define parser generic-parameter-list (<token>)
   rule seq(lex-LF-PAREN, opt(parameters), lex-RT-PAREN,
            opt-seq(lex-ARROW, generic-values))
   => tokens;
   slot parameter-list = parameter-list-from-token(tokens[1]);
   slot value-list = (tokens[3] & tokens[3][1].value-list) | #[];
attributes
   // All generic-parameter-list parts have these followers and recovery.
   type-followers = vector(parse-lex-COMMA, parse-lex-RT-PAREN),
   type-skipper = parse-til-rt-paren,
   expression-followers = vector(parse-lex-COMMA, parse-lex-RT-PAREN),
   expression-skipper = parse-til-rt-paren
end;

define parser generic-values (<token>)
   rule choice(bare-values-list, enclosed-values-list) => token;
   slot value-list = value-list-from-token(token);
end;

define parser generic-options :: <sequence> /* of <property-token> */
   rule comma-property-list => token;
   yield token;
end;

//
// Modules & libraries
//

define parser module-definer (<definition-token>)
   rule seq(lex-MODULE, lex-NAME, opt(module-clauses), lex-END) => tokens;
   inherited slot api-name = tokens[1].value;
   slot namespace-clauses = tokens[2] | #[];
afterwards (context, tokens, value, start-pos, end-pos)
   note-combined-source-location(context, value, tokens);
end;

define parser library-definer (<definition-token>)
   rule seq(lex-LIBRARY, lex-NAME, opt(library-clauses), lex-END) => tokens;
   inherited slot api-name = tokens[1].value;
   slot namespace-clauses = tokens[2] | #[];
afterwards (context, tokens, value, start-pos, end-pos)
   note-combined-source-location(context, value, tokens);
end;

define parser module-clauses :: <sequence>
   rule seq(module-clause, opt-many(seq(lex-SEMICOLON, module-clause)),
            opt(lex-SEMICOLON))
   => tokens;
   yield list-from-tokens(tokens);
end;

define parser module-clause :: <token>
   rule choice(export-clause, create-clause, use-clause) => token;
   yield token;
end;

define parser library-clauses :: <sequence>
   rule seq(library-clause, opt-many(seq(lex-SEMICOLON, library-clause)),
            opt(lex-SEMICOLON))
   => tokens;
   yield list-from-tokens(tokens);
end;

define parser library-clause :: <token>
   rule choice(export-clause, use-clause) => token;
   yield token;
end;

define parser export-clause (<source-location-token>)
   rule seq(lex-EXPORT, lex-ORDINARY-NAME, opt-many(seq(lex-COMMA, lex-ORDINARY-NAME)),
            opt(lex-COMMA))
   => tokens;
   slot export-names = map(value, list-from-tokens(copy-sequence(tokens, start: 1)));
afterwards (context, tokens, value, start-pos, end-pos)
   note-combined-source-location(context, value, tokens);
end;

define parser create-clause (<source-location-token>)
   rule seq(lex-CREATE, lex-ORDINARY-NAME, opt-many(seq(lex-COMMA, lex-ORDINARY-NAME)),
            opt(lex-COMMA))
   => tokens;
   slot create-names = map(value, list-from-tokens(copy-sequence(tokens, start: 1)));
afterwards (context, tokens, value, start-pos, end-pos)
   note-combined-source-location(context, value, tokens);
end;

define parser use-clause (<source-location-token>)
   rule seq(lex-USE, lex-ORDINARY-NAME, opt-many(seq(lex-COMMA, use-option)),
            opt(lex-COMMA))
   => tokens;
   slot use-name = tokens[1].value;
   slot use-imports = #f;
   slot use-exclusions = #f;
   slot use-prefix = #f;
   slot use-renamings = #f;
   slot use-exports = #f;
afterwards (context, tokens, value, start-pos, end-pos)
   let options = collect-subelements(tokens[2], 1);
   for (option in options)
      select (option.object-class)
         <import-option-token> => value.use-imports := option.import-renamings;
         <exclude-option-token> => value.use-exclusions := option.exclude-names;
         <prefix-option-token> => value.use-prefix := option.prefix-text;
         <rename-option-token> => value.use-renamings := option.rename-renamings;
         <export-option-token> => value.use-exports := option.export-names;
      end select;
   end for;
   note-combined-source-location(context, value, tokens);
end;

define parser use-option :: <token>
   rule choice(import-option, exclude-option, prefix-option, rename-option, 
               export-option)
   => token;
   yield token;
end;

define parser import-option (<token>)
   rule seq(lex-IMPORT-SYM,
            choice(seq(nil(#"all"), lex-ALL),
                   seq(nil(#"list"), lex-LF-BRACE, opt(var-mod-list), lex-RT-BRACE)))
   => tokens;
   slot import-renamings :: type-union(singleton(#"all"), <sequence>) =
      select (tokens[1][0])
         #"all" => #"all";
         #"list" => tokens[1][2];
      end select;
end;

define parser var-mod-list :: <sequence> /* of <renaming-token> */
   rule seq(var-mod-spec, opt-many(seq(lex-COMMA, var-mod-spec)), opt(lex-COMMA))
   => tokens;
   yield list-from-tokens(tokens);
end;

define parser var-mod-spec (<renaming-token>)
   rule seq(lex-NAME, opt-seq(lex-ARROW, lex-NAME)) => tokens;
   inherited slot import-name = tokens[0].value;
   inherited slot local-name = tokens[1] & tokens[1][1].value;
end;

define parser exclude-option (<token>)
   rule seq(lex-EXCLUDE-SYM, lex-LF-BRACE, opt(name-list), lex-RT-BRACE) => tokens;
   slot exclude-names = tokens[2];
end;

define parser name-list :: <sequence> /* of <string> */
   rule seq(lex-NAME, opt-many(seq(lex-COMMA, lex-NAME)), opt(lex-COMMA))
   => tokens;
   yield map(value, list-from-tokens(tokens));
end;

define parser prefix-option (<token>)
   rule seq(lex-PREFIX-SYM, string-literal) => tokens;
   slot prefix-text = tokens[1].value;
end;

define parser rename-option (<token>)
   rule seq(lex-RENAME-SYM, lex-LF-BRACE, opt(renaming-list), lex-RT-BRACE) => tokens;
   slot rename-renamings = tokens[2];
end;

define parser renaming-list :: <sequence> /* of <renaming-token> */
   rule seq(renaming, opt-many(seq(lex-COMMA, renaming)), opt(lex-COMMA))
   => tokens;
   yield list-from-tokens(tokens);
end;

define parser renaming (<source-location-token>)
   rule seq(lex-NAME, lex-ARROW, lex-NAME) => tokens;
   slot import-name = tokens[0].value;
   slot local-name = tokens[2].value;
afterwards (context, tokens, value, start-pos, end-pos)
   note-combined-source-location(context, value, tokens);
end;

define parser export-option (<token>)
   rule seq(lex-EXPORT-SYM,
            choice(seq(nil(#"all"), lex-ALL),
                   seq(nil(#"list"), lex-LF-BRACE, opt(name-list), lex-RT-BRACE)))
   => tokens;
   slot export-names :: type-union(singleton(#"all"), <sequence>) =
      select (tokens[1][0])
         #"all" => #"all";
         #"list" => tokens[1][2];
      end select;
end;

//
// Sealed domain
//

define parser domain-definer (<definition-token>)
   rule seq(lex-SEALED, lex-DOMAIN, variable-name,
            lex-LF-PAREN, opt(type-list), lex-RT-PAREN)
   => tokens;
afterwards (context, tokens, value, start-pos, end-pos)
   note-combined-source-location(context, value, tokens);
end;

define parser type-list (<token>)
   rule seq(expression, opt-many(seq(lex-COMMA, expression)), lex-COMMA)
   => tokens;
end;

//
// Macros
//

define parser macro-definer (<definition-token>)
   rule seq(lex-MACRO, lex-NAME) => tokens;
   // TODO: Skip rules and patterns so they aren't treated like actual definitions.
afterwards (context, tokens, value, start-pos, end-pos)
   note-combined-source-location(context, value, tokens);
end;

module: markup-parser

//
// Types
//

define constant <markup-word-types> =
      type-union(<quote-token>,
                 <api-ref-token>,
                 <line-marker-ref-token>,
                 <footnote-ref-token>,
                 <image-ref-token>,
                 <synopsis-ref-token>,
                 <bracketed-render-span-token>,
                 <text-word-token>);

define constant <title-word-types> =
      type-union(<quote-token>,
                 <image-ref-token>,
                 <bracketed-render-span-token>,
                 <text-word-token>);

// These result in an error if actually used in type specifications.
// define constant <markup-word-sequence> =
//       limited(<sequence>, of: <markup-word-types>);
// 
// define constant <link-word-sequence> =
//       limited(<sequence>, of: <link-word-token>);

define constant <markup-word-sequence> = <sequence>;
define constant <link-word-sequence> = <sequence>;

//
// Markup & title words
//

define caching parser markup-words :: <markup-word-sequence>
   rule many(seq(markup-word, opt-spaces)) => items;
   yield collect-subelements(items, 0);
end;

define caching parser markup-words-til-hyphen-spc :: <markup-word-sequence>
   rule many(seq(not-next(hyphen-spc), markup-word, opt-spaces)) => items;
   yield collect-subelements(items, 1);
end;

define caching parser markup-word :: <markup-word-types>
   rule choice(quote, api-ref, line-marker-ref, footnote-ref, image-ref,
               synopsis-ref, bracketed-render-span, text-word)
      => token;
   yield token;
end;

define caching parser title-word :: <title-word-types>
   rule seq(not-next(ascii-line),
            choice(quote, image-ref, bracketed-render-span, text-word))
      => tokens;
   yield tokens[1];
end;

// exported
define caching parser text-word (<source-location-token>)
   rule text-til-spc-ls => token;
   slot text :: <string> = token.text;
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

define caching parser word-line :: <text-word-token>
   rule seq(sol, text-word, ls) => tokens;
   yield tokens[1];
end;

// exported as <text-word-token>
define caching parser word-til-cls-brack (<text-word-token>)
   rule text-til-spc-cls-brack => token;
   inherited slot text = token.text;
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

//
// Links
//

define caching parser indented-link-words :: <link-word-sequence>
   rule seq(indent, link-word-lines, dedent) => tokens;
   yield tokens[1];
end;

define caching parser link-word-lines :: <link-word-sequence>
   rule many(seq(sol, link-words, ls)) => tokens;
   yield integrate-sequences(collect-subelements(tokens, 1));
end;

define caching parser link-words :: <link-word-sequence>
   rule seq(link-word, opt-many(seq(spaces, link-word))) => tokens;
   yield first-item-and-last-subelements(tokens);
end;

// exported
define caching parser link-word (<source-location-token>)
   rule choice(seq(quote-start, text-til-end-quote, quote-end),
               seq(nil(#f), text-til-spc-ls))
      => token;
   slot text :: <string> = remove-multiple-spaces(token[1].text);
attributes
   close-quote-char :: false-or(<character>) = #f;
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

// exported as <link-word-token>
define caching parser link-line (<link-word-token>)
   rule choice(seq(sol, quote-start, text-til-end-quote, quote-end, ls),
               seq(sol, nil(#f), text-til-ls, ls))
      => token;
   inherited slot text = remove-multiple-spaces(token[2].text);
attributes
   close-quote-char :: false-or(<character>) = #f;
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

// exported as <link-word-token>
define caching parser link-til-cls-brack (<link-word-token>)
   rule choice(seq(quote-start, text-til-end-quote, quote-end),
               seq(nil(#f), text-til-cls-brack))
      => token;
   inherited slot text = remove-multiple-spaces(token[1].text);
attributes
   close-quote-char :: false-or(<character>) = #f;
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

//
// Names & API names
//

define caching parser filename :: <string>
   rule seq(quote-start, text-til-end-quote, quote-end) => tokens;
   yield tokens[1].text;
attributes
   close-quote-char :: false-or(<character>) = #f;
end;

define caching parser nickname-word :: <string>
   rule text-til-cls-brack => token;
   yield remove-multiple-spaces(token.text);
end;

// exported
define caching parser api-ref (<source-location-token>)
   label "API reference";
   rule nil(#f) => token;
afterwards (context, tokens, value, start-pos, end-pos, fail: fail)
   fail(make(<parse-failure>));
end;

//
// Text tokens
//

define caching parser text-til-ls (<token>)
   rule many(seq(not-next(ls), char)) => items;
   slot text :: <string> = as(<string>, collect-subelements(items, 1));
end;

define caching parser text-til-spc-ls (<token>)
   rule many(seq(not-next(spc-ls), char)) => items;
   slot text :: <string> = as(<string>, collect-subelements(items, 1));
end;

define caching parser text-til-ascii-nickname-ls (<token>)
   rule many(seq(not-next(choice(ls,
                                 seq(spaces, ascii-line),
                                 seq(spaces, title-nickname))),
                 char))
      => items;
   slot text :: <string> = as(<string>, collect-subelements(items, 1));
end;

define caching parser text-til-cls-brack (<token>)
   rule many(seq(not-next(spc-cls-brack), char)) => items;
   slot text :: <string> =
      replace-ls-with-spc(as(<string>, collect-subelements(items, 1)));
end;

define caching parser text-til-spc-cls-brack (<token>)
   rule many(seq(not-next(spc-ls), not-next(close-bracket), char)) => items;
   slot text :: <string> = as(<string>, collect-subelements(items, 2));
end;

define caching parser text-til-end-quote (<token>)
   rule many(seq(not-next(quote-end), char)) => items;
   slot text :: <string> =
      replace-ls-with-spc(as(<string>, collect-subelements(items, 1)));
end;

//
// Bracketed render span
//

// exported
define caching parser bracketed-render-span (<source-location-token>)
   rule seq(bracketed-render-span-start,
            opt-many(seq(not-next(bracketed-render-span-end), char)),
            bracketed-render-span-end)
      => tokens;
   slot block-type :: <symbol> = tokens[0];
   slot text :: <string> = as(<string>, collect-subelements(tokens[1], 1));
attributes
   bracketed-spec-text :: false-or(<string>) = #f;
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

define caching parser bracketed-render-span-start :: <symbol>
   rule seq(opn-brack-spc, bracketed-render-span-spec-text, spc-cls-brack)
      => tokens;
   yield tokens[1];
afterwards (context, tokens, value, start-pos, end-pos)
   attr(bracketed-spec-text) := tokens[1];
end;

define caching parser bracketed-render-span-end
   rule seq(opn-brack-spc, end-lit,
            opt-seq(many-spc-ls, bracketed-render-span-spec-text),
            spc-cls-brack);
afterwards (context, tokens, value, start-pos, end-pos, fail: fail)
   check-end-spec-text(tokens[2] & tokens[2][1], fail)
end;

//
// Literals
//

define caching parser directive-topic-spec-text :: <symbol>
   rule choice(seq(nil(#f), constant-lit),
               seq(nil(#f), function-lit),
               seq(nil(#f), variable-lit),
               seq(generic-lit, spaces, function-lit),
               seq(nil(#f), library-lit),
               seq(nil(#f), method-lit),
               seq(nil(#f), module-lit),
               seq(nil(#f), class-lit),
               seq(nil(#f), macro-lit),
               seq(nil(#f), topic-lit))
      => token;
   yield select (token[0])
            #"generic" => #"generic-function";
            otherwise => token[1];
         end select;
end;

define caching parser titled-directive-section-spec-text :: <symbol>
   rule section-lit => token;
   yield #"section";
end;

define caching parser paragraph-directive-spec-text :: <symbol>
   rule choice(synopsis-lit, syn-lit) => token;
   yield #"synopsis";
end;

define caching parser link-directive-spec-text :: <symbol>
   rule seq(parent-lit, opt-seq(spaces, topic-lit)) => tokens;
   yield #"parent";
end;

define caching parser links-directive-spec-text :: <symbol>
   rule choice(seq(relevant-lit, spaces, to-lit),
               seq(see-lit, spaces, also-lit))
      => token;
   yield select (token[0])
            #"relevant" => #"relevant-to";
            #"see" => #"see-also";
         end select;
end;

define caching parser word-directive-spec-text :: <symbol>
   rule seq(fully-lit, spaces, qualified-lit, spaces, name-lit) => tokens;
   yield #"fully-qualified-name";
end;

define caching parser division-directive-spec-text :: <symbol>
   rule choice(seq(nil(#"keywords"),   init-keywords-lit),
               seq(nil(#"conditions"), conditions-lit),
               seq(nil(#"conditions"), exceptions-lit),
               seq(nil(#"arguments"),  arguments-lit),
               seq(nil(#"keywords"),   keywords-lit),
               seq(nil(#"conditions"), signals-lit),
               seq(nil(#"conditions"), errors-lit),
               seq(nil(#"values"),     values-lit), 
               seq(nil(#"arguments"),  args-lit),
               seq(nil(#"keywords"),   make-lit, spaces, keywords-lit))
      => token;
   yield token[0];
end;

define caching parser indented-content-directive-spec-text :: <symbol>
   rule choice(warning-lit, note-lit) => token;
   yield token;
end;

define caching parser null-directive-spec-text
   rule discussion-lit;
end;

define caching parser api-list-spec-text :: <symbol>
   rule choice(seq(nil(#f), functions-lit),
               seq(nil(#f), libraries-lit),
               seq(nil(#f), variables-lit), 
               seq(nil(#f), bindings-lit),
               seq(nil(#f), classes-lit),
               seq(nil(#f), modules-lit), 
               seq(unbound-lit, spaces, names-lit),
               seq(nil(#f), macros-lit))
      => token;
   yield select (token[0])
            #"unbound" => #"unbound-names";
            otherwise => token[1];
         end select;
end;

define caching parser bracketed-raw-block-spec-text :: <symbol>
   rule choice(verbatim-lit, diagram-lit, example-lit, code-lit) => token;
   yield token;
end;

define caching parser bracketed-render-span-spec-text
   rule choice(dita-lit, html-lit) => token;
   yield token;
end;

//
// Special characters
//

define parser-method ascii-line-char (stream, context)
=> (char :: false-or(<character>))
   label format-to-string("line character (%s)", $ascii-line-chars);
   let char = read-element(stream, on-end-of-stream: #f);
   member?(char, $ascii-line-chars) & char
end;

define parser-method bullet-char (stream, context)
=> (char :: false-or(<character>))
   label format-to-string("bullet character (%s)", $bullet-chars);
   let char = read-element(stream, on-end-of-stream: #f);
   member?(char, $bullet-chars) & char
end;

//
// Convenience
//

define caching parser spc-ls
   rule choice(spc, ls)
end;

define caching parser opn-brack-spc
   rule seq(open-bracket, opt-many-spc-ls)
end;

define caching parser spc-cls-brack
   rule seq(opt-many-spc-ls, close-bracket)
end;

define caching parser many-spc-ls
   rule many(spc-ls)
end;

define caching parser opt-many-spc-ls
   rule opt-many(spc-ls)
end;

define caching parser spaces
   rule many(spc)
end;

define caching parser opt-spaces
   rule opt-many(spc)
end;

define parser sol (<token>)
   rule opt-spaces => token
end;

define parser indent-dedent
   rule opt-many(choice(indent, dedent))
end;
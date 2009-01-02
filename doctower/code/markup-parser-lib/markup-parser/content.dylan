module: markup-parser

//
// Types
//

// This results in an error if actually used in type specifications.
// define constant <raw-line-sequence> = limited(<sequence>, of: <raw-line-token>);

define constant <raw-line-sequence> = <sequence>;

//
// Content
//

// #"blank-lines" counts as #f
define caching parser content-block :: type-union(<division-content-types>, singleton(#f))
   rule choice(blank-lines, marginal-code-block, marginal-verbatim-block,
               figure-ref-line, content-ref-line, ditto-ref-line,
               api-list-ref-line, bracketed-raw-block, table, bullet-list,
               numeric-list, hyphenated-list, phrase-list, indented-content-directive,
               paragraph)
      => token;
   yield (token ~= #"blank-lines") & token;
end;

define caching parser blank-lines
   rule many(seq(opt-spaces, ls))
end;

//
// Marginal blocks
//

// exported
define caching parser marginal-code-block (<source-location-token>)
   rule many(marginal-code-block-line) => lines;
   slot content :: <raw-line-sequence> = lines;
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

define caching parser marginal-code-block-line :: <raw-line-token>
   rule seq(colon, spc, raw-line) => tokens;
   yield tokens[2];
end;

// exported
define caching parser marginal-verbatim-block (<source-location-token>)
   rule many(marginal-verbatim-block-line) => lines;
   slot content :: <raw-line-sequence> = lines;
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

define caching parser marginal-verbatim-block-line :: <raw-line-token>
   rule seq(choice(gt, bar), spc, raw-line) => tokens;
   yield tokens[2];
end;

//
// References
//

// exported
define caching parser figure-ref-line (<source-location-token>)
   rule seq(opn-brack-spc, fig-lit, many-spc-ls, filename,
            opt-seq(many-spc-ls, scale-factor), spc-cls-brack,
            opt(text-til-ls), ls)
      => tokens;
   slot filename :: <string> = tokens[2];
   slot scale-factor :: false-or(<integer>) = tokens[3] & tokens[3][1].factor;
   slot scale-type :: false-or(<symbol>) = tokens[3] & tokens[3][1].type;
   slot caption :: false-or(<string>) =
      tokens[5] & remove-multiple-spaces(tokens[5].text);
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

define caching parser scale-factor (<token>)
   rule seq(number, choice(percent, x-lit)) => tokens;
   slot factor :: <integer> = tokens[0];
   slot type :: <symbol> = tokens[1];
end;

// exported -- link can be false if referring to sub-topics of current topic
define caching parser content-ref-line (<source-location-token>)
   rule seq(opn-brack-spc, contents-lit,
            opt-seq(many-spc-ls, of-lit, many-spc-ls, link-til-cls-brack),
            spc-cls-brack, ls)
      => tokens;
   slot link :: false-or(<link-word-token>) = tokens[2] & tokens[2][3];
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

// exported
define caching parser ditto-ref-line (<source-location-token>)
   rule seq(opn-brack-spc, ditto-lit, many-spc-ls, link-til-cls-brack, spc-cls-brack, ls)
      => tokens;
   slot link :: <link-word-token> = tokens[3];
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

// exported
define caching parser api-list-ref-line (<source-location-token>)
   rule seq(opn-brack-spc, list-lit, many-spc-ls, of-lit, many-spc-ls,
            api-list-spec-text, spc-cls-brack, ls)
      => tokens;
   slot list-type :: <symbol> = tokens[5];
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

// exported
define caching parser bracketed-raw-block (<source-location-token>)
   rule seq(bracketed-raw-block-start-line,
            opt-many(seq(not-next(bracketed-raw-block-end-line), raw-line)),
            bracketed-raw-block-end-line)
      => tokens;
   slot block-type :: <symbol> = tokens[0];
   slot content :: <raw-line-sequence> = collect-subelements(tokens[1], 1) | #[];
attributes
   bracketed-spec-text :: false-or(<string>) = #f;
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

define caching parser bracketed-raw-block-start-line :: <symbol>
   rule seq(opn-brack-spc, bracketed-raw-block-spec-text, spc-cls-brack, ls)
      => tokens;
   yield tokens[1];
afterwards (context, tokens, value, start-pos, end-pos)
   attr(bracketed-spec-text) := tokens[1];
end;

define caching parser bracketed-raw-block-end-line
   rule seq(opn-brack-spc, end-lit,
            opt-seq(many-spc-ls, bracketed-raw-block-spec-text),
            spc-cls-brack, ls);
afterwards (context, tokens, value, start-pos, end-pos, fail: fail)
   check-end-spec-text(tokens[2] & tokens[2][1], fail)
end;

//
// Tables
//

// exported
define caching parser table (<token>)
   label "table";
   rule seq(table-header, many(table-row), table-footer) => tokens;
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

define caching parser table-header
   rule nil(#f);
afterwards (context, tokens, value, start-pos, end-pos, fail: fail)
   fail(make(<parse-failure>))
end;

define caching parser table-row
   rule nil(#f);
end;

define caching parser table-footer
   rule nil(#f);
end;

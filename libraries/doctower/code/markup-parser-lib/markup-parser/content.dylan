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
define caching parser content-block :: false-or(<division-content-types>)
   rule choice(blank-lines, marginal-code-block, marginal-verbatim-block,
               figure-ref-line, ditto-ref-line, api-list-ref-line,
               bracketed-raw-block, table, bullet-list, numeric-list,
               hyphenated-list, phrase-list, indented-content-directive,
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
   rule seq(sol, colon, spc, raw-line) => tokens;
   yield tokens[3];
end;

// exported
define caching parser marginal-verbatim-block (<source-location-token>)
   rule many(marginal-verbatim-block-line) => lines;
   slot content :: <raw-line-sequence> = lines;
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

define caching parser marginal-verbatim-block-line :: <raw-line-token>
   rule seq(sol, choice(gt, bar), spc, raw-line) => tokens;
   yield tokens[3];
end;

//
// References
//

// exported
define caching parser figure-ref-line (<source-location-token>)
   rule seq(sol, opn-brack-spc, fig-lit, many-spc-ls, filename,
            opt-seq(many-spc-ls, scale-factor), spc-cls-brack,
            opt(text-til-ls), ls)
      => tokens;
   slot filename :: <string> = tokens[3];
   slot scale-factor :: false-or(<integer>) = tokens[4] & tokens[4][1].factor;
   slot scale-type :: false-or(<symbol>) = tokens[4] & tokens[4][1].type;
   slot caption :: false-or(<string>) =
      tokens[6] & remove-multiple-spaces(tokens[6].text);
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

define caching parser scale-factor (<token>)
   rule seq(number, choice(percent, x-lit)) => tokens;
   slot factor :: <integer> = tokens[0];
   slot type :: <symbol> = tokens[1];
end;

// exported
define caching parser ditto-ref-line (<source-location-token>)
   rule seq(sol, opn-brack-spc, ditto-lit, many-spc-ls, link-til-cls-brack,
            spc-cls-brack, ls)
      => tokens;
   slot link :: <link-word-token> = tokens[4];
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

// exported
define caching parser api-list-ref-line (<source-location-token>)
   rule seq(sol, opn-brack-spc,
            list-lit, many-spc-ls, of-lit, many-spc-ls, api-list-spec-text,
            opt-seq(many-spc-ls, in-lit, many-spc-ls, word-til-cls-brack),
            spc-cls-brack, ls)
      => tokens;
   slot list-type :: <symbol> = tokens[6];
   slot scope-word :: false-or(<text-word-token>) = tokens[7] & tokens[7][3];
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
   raw-leading-spaces :: <integer> = 0,
   bracketed-spec-text :: false-or(<symbol>) = #f;
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

define caching parser bracketed-raw-block-start-line :: <symbol>
   rule seq(sol, opn-brack-spc, bracketed-raw-block-spec-text, spc-cls-brack, ls)
      => tokens;
   yield tokens[2];
afterwards (context, tokens, value, start-pos, end-pos)
   attr(raw-leading-spaces) := tokens[0].parse-end - tokens[0].parse-start;
   attr(bracketed-spec-text) := tokens[2];
end;

define caching parser bracketed-raw-block-end-line
   rule seq(indent-dedent, sol, opn-brack-spc, end-lit,
            opt-seq(many-spc-ls, bracketed-raw-block-spec-text),
            spc-cls-brack, ls);
afterwards (context, tokens, value, start-pos, end-pos, fail: fail)
   check-end-spec-text(tokens[4] & tokens[4][1], fail)
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

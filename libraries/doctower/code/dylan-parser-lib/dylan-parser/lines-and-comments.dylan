module: dylan-parser
synopsis: Line-oriented parsing and grammar of documentation comments.


// Doc comments only need parse-start and parse-end; content is parsed separately.
// Span includes opening comment delimiter and all text up to and including 
// closing comment delimiter or eol, including leading spaces and eol comment
// delimiters on each line.
define class <doc-comment-token> (<source-location-token>)
end class;


//
// Line oriented parsing
//

define parser source-record (<source-location-token>)
   rule seq(opt-many(seq(opt(lines-til-parsable), choice(definition, doc-block))),
            opt(lines-til-parsable))
   => tokens;
   slot definitions = source-record-definitions(tokens[0] | #[]);
   slot unscoped-docs = attr(scoped-docs);
attributes
   scoped-docs = make(<stretchy-vector>);
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value);
end;

define parser lines-til-parsable
   rule many(seq(not-next(definition), not-next(doc-block), line));
end;


//
// Lexical whitespace
//

define caching parser opt-spaces
   rule opt-many(spc)
end;

define parser whitespace (<token>)
   label "comment or whitespace";
   rule many(choice(spc, eol, doc-block, comment)) => tokens;
afterwards (context, tokens, value, start-pos, end-pos)
   let docs = tokens.choose-doc-comments;
   context.last-whitespace-doc := ~docs.empty? & docs.last;
end;

define parser opt-whitespace :: false-or(<whitespace-token>)
   rule opt(whitespace) => token;
   yield token;
end;

define parser lex-EOF
   rule seq(opt-whitespace, not-next(char))
end;


//
// Comments
//

define caching parser comment
   rule choice(delim-comment, eol-comment);
end;

define parser delim-comment
   rule seq(lf-comment, opt-many(choice(delim-comment, delim-comment-text)),
            rt-comment)
end;

define parser eol-comment
   rule seq(double-slash, opt(eol-comment-text), eol)
end;

define caching parser doc-block :: <doc-comment-token>
   rule seq(opt-spaces, choice(delim-doc-comment, eol-doc-comments)) => tokens;
   yield tokens[1];
afterwards (context, tokens, value, start-pos, end-pos)
   add-new!(attr(scoped-docs), tokens[1], test: \=)
end;

define parser delim-doc-comment (<doc-comment-token>)
   rule seq(lf-doc-comment, spc,
            opt-many(choice(delim-comment, delim-comment-text)),
            rt-comment)
   => tokens;
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value);
end;

define parser eol-doc-comments (<doc-comment-token>)
   rule many(seq(opt-spaces, eol-doc-comment)) => tokens;
afterwards (context, tokens, value, start-pos, end-pos)
   note-combined-source-location(context, value, tokens);
end;

define parser eol-doc-comment (<token>)
   rule seq(triple-slash, opt-seq(spc, eol-comment-text), eol) => tokens;
end;

define parser delim-comment-text
   rule many(seq(not-next(rt-comment), char))
end;

define parser eol-comment-text
   rule many(seq(not-next(eol), char))
end;

module: markup-parser

//
// Bullet lists
//

// exported
define caching parser bullet-list (<source-location-token>)
   rule many(seq(bullet-list-item, opt(blank-lines)))
      => items;
   slot content :: <sequence> /* of <bullet-list-item-token> */ =
      collect-subelements(items, 0);
attributes
   bullet-char :: <character> = ' ';
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

// exported
define caching parser bullet-list-item (<source-location-token>)
   rule seq(bullet-list-marker, remainder-and-indented-content) => tokens;
   slot content :: <division-content-sequence> = tokens[1];
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

define caching parser bullet-list-marker
   rule seq(bullet-char, spaces);
afterwards (context, tokens, value, start-pos, end-pos, fail: fail)
   check-bullet-char(tokens[0], fail)
end;

//
// Numeric list
//

// exported
define caching parser numeric-list (<source-location-token>)
   rule seq(numeric-list-first-item, opt-many(seq(opt(blank-lines), numeric-list-item)))
      => items;
   slot list-start :: type-union(<integer>, <string>) = items[0].ordinal;
   slot content :: <sequence> /* of <numeric-list-item-token> */ =
      first-item-and-last-subelements(items);
attributes
   ordinal-type :: false-or(<class>) = #f,
   ordinal-separator :: false-or(<symbol>) = #f;
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

// exported as <numeric-list-item-token>
define caching parser numeric-list-first-item (<numeric-list-item-token>)
   rule seq(numeric-list-first-marker, remainder-and-indented-content) => tokens;
   slot ordinal :: type-union(<integer>, <string>) = tokens[0].ordinal;
   inherited slot content /* :: <division-content-sequence> */ = tokens[1];
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

// exported
define caching parser numeric-list-item (<source-location-token>)
   rule seq(numeric-list-marker, remainder-and-indented-content) => tokens;
   slot content :: <division-content-sequence> = tokens[1];
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

define caching parser numeric-list-first-marker :: type-union(<integer>, <string>)
   rule seq(choice(number, ordinal), choice(colon, right-paren, period), spaces)
      => tokens;
   yield tokens[0];
afterwards (context, tokens, value, start-pos, end-pos)
   attr(ordinal-type) := tokens[0].object-class;
   attr(ordinal-separator) := tokens[1];
end;

define caching parser numeric-list-marker
   rule seq(choice(number, ordinal, hash), choice(colon, right-paren, period), spaces);
afterwards (context, tokens, value, start-pos, end-pos, fail: fail)
   check-numeric-list-marker(tokens, fail);
end;

//
// Hyphenated list
//

// exported
define caching parser hyphenated-list (<source-location-token>)
   rule many(seq(hyphenated-list-item, opt(blank-lines))) => items;
   slot content :: <sequence> /* of <hyphenated-list-item-token> */ =
      collect-subelements(items, 0);
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

// exported
define caching parser hyphenated-list-item (<source-location-token>)
   rule seq(hyphenated-list-label, remainder-and-indented-content) => tokens;
   slot item-label :: <markup-word-sequence> = tokens[0];
   slot content :: <division-content-sequence> = tokens[1];
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

define caching parser hyphenated-list-label :: <markup-word-sequence>
   rule seq(markup-words-til-hyphen-spc, hyphen-spc) => tokens;
   yield tokens[0];
end;

define caching parser hyphen-spc
   rule seq(hyphen, spaces);
end;

//
// Phrase list
//

// exported
define caching parser phrase-list (<source-location-token>)
   rule many(seq(phrase-list-item, opt(blank-lines))) => items;
   slot content :: <sequence> /* of <phrase-list-item-token> */ =
      collect-subelements(items, 0);
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

// exported
define caching parser phrase-list-item (<source-location-token>)
   rule seq(phrase-list-label, indented-content) => tokens;
   slot item-label :: <markup-word-sequence> = tokens[0];
   slot content :: <division-content-sequence> = tokens[1];
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

define caching parser phrase-list-label :: <markup-word-sequence>
   rule seq(paragraph-til-hyphen-ls, hyphen-ls) => tokens;
   yield tokens[0].content;
end;

define caching parser hyphen-ls
   rule seq(hyphen, ls)
end parser;

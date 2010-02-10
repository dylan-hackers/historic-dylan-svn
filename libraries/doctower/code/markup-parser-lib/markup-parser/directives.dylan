module: markup-parser

//
// Directive topics
//

// exported
define caching parser directive-topic-title (<source-location-token>)
   rule seq(directive-spec-intro, directive-topic-spec-text, colon, spaces,
            text-til-ascii-nickname-ls, opt-seq(spaces, ascii-midline),
            choice(seq(ls, nil(#f)), seq(spaces, title-nickname)),
            opt(ascii-underline))
      => tokens;
   slot title-type :: <symbol> = tokens[1];
   slot title-text :: <string> = remove-multiple-spaces(tokens[4].text);
   slot title-nickname :: false-or(<title-nickname-token>) = tokens[6][1];
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

//
// Section directives
//

// exported
define caching parser paragraph-directive (<source-location-token>)
   rule seq(paragraph-directive-spec, opt(markup-words), ls,
            opt(paragraph-til-null-directive))
      => tokens;
   slot directive-type :: <symbol> = tokens[0];
   slot content :: <division-content-sequence> =
      prepend-words(tokens[1], tokens[3]);
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

// exported
define caching parser link-directive (<source-location-token>)
   rule seq(link-directive-spec, link-line) => tokens;
   slot directive-type :: <symbol> = tokens[0];
   slot link :: <link-word-token> = tokens[1];
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

// exported
define caching parser links-directive (<source-location-token>)
   rule seq(links-directive-spec, opt(link-words), ls, opt(indented-link-words))
      => tokens;
   slot directive-type :: <symbol> = tokens[0];
   slot links :: <link-word-sequence> =
      concatenate(tokens[1] | #[], tokens[3] | #[]);
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

// exported
define caching parser word-directive (<source-location-token>)
   rule seq(word-directive-spec, word-line) => tokens;
   slot directive-type :: <symbol> = tokens[0];
   slot word :: <text-word-token> = tokens[1];
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

// exported
define caching parser division-directive (<source-location-token>)
   rule seq(division-directive-spec, opt(division-content))
      => tokens;
   slot directive-type :: <symbol> = tokens[0];
   slot content :: <division-content-sequence> = tokens[1];
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

// exported
define caching parser indented-content-directive (<source-location-token>)
   rule seq(indented-content-directive-spec, opt(remainder-and-indented-content))
      => tokens;
   slot directive-type :: <symbol> = tokens[0];
   slot content :: <division-content-sequence> = tokens[1];
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

define caching parser null-directive :: <division-content-sequence>
   rule seq(null-directive-spec, opt(division-content))
      => tokens;
   yield tokens[1];
end;

//
// Directive specs
//

define caching parser paragraph-directive-spec :: <symbol>
   rule seq(directive-spec-intro, paragraph-directive-spec-text, colon,
            directive-spec-outro)
      => tokens;
   yield tokens[1];
end;

define caching parser link-directive-spec :: <symbol>
   rule seq(directive-spec-intro, link-directive-spec-text, colon,
            directive-spec-outro)
      => tokens;
   yield tokens[1];
end;

define caching parser links-directive-spec :: <symbol>
   rule seq(directive-spec-intro, links-directive-spec-text, colon,
            directive-spec-outro)
      => tokens;
   yield tokens[1];
end;

define caching parser word-directive-spec :: <symbol>
   rule seq(directive-spec-intro, word-directive-spec-text, colon,
            directive-spec-outro)
      => tokens;
   yield tokens[1];
end;

define caching parser division-directive-spec :: <symbol>
   rule seq(directive-spec-intro, division-directive-spec-text, colon,
            directive-spec-outro)
      => tokens;
   yield tokens[1];
end;

define caching parser indented-content-directive-spec :: <symbol>
   rule seq(directive-spec-intro, indented-content-directive-spec-text, colon,
            directive-spec-outro)
      => tokens;
   yield tokens[1];
end;

define caching parser null-directive-spec :: <symbol>
   rule seq(directive-spec-intro, null-directive-spec-text, colon,
            directive-spec-outro)
      => tokens;
   yield tokens[1];
end;

define caching parser directive-spec-intro
   rule seq(opt(ascii-overline), opt-seq(sol, ascii-midline, spaces))
end;

define caching parser directive-spec-outro
   rule choice(seq(spaces, opt-seq(ascii-midline, ls, opt(ascii-underline))),
               seq(ls, opt(ascii-underline)))
end;

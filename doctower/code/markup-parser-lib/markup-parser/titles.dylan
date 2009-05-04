module: markup-parser

//
// Types
//

// This results in an error if actually used in type specifications.
// define constant <title-word-sequence> =
//       limited(<sequence>, of: <title-word-types>);

define constant <title-word-sequence> = <sequence>;

//
// Titled topics and sections
//

define caching parser topic-title :: <topic-or-section-title-token>
   label "topic title";
   rule topic-or-section-title => token;
   yield token;
afterwards (context, token, value, start-pos, end-pos, fail: fail)
   unless (token.title-style ~= $section-style)
      fail(make(<parse-failure>))
   end unless;
end;
   
define caching parser section-title :: <topic-or-section-title-token>
   label "section title";
   rule topic-or-section-title => token;
   yield token;
afterwards (context, token, value, start-pos, end-pos, fail: fail)
   unless (token.title-style = $section-style)
      fail(make(<parse-failure>))
   end unless;
end;

// exported
define caching parser topic-or-section-title (<source-location-token>)
   rule choice(title-midline-style, title-bare-style) => token;
   slot title-style :: <topic-level-style> = topic-level-style-from-attributes();
   slot title-content :: <title-word-sequence> = token.title-content;
   slot title-nickname :: false-or(<title-nickname-token>) = token.title-nickname;
attributes
   title-line-char :: <character> = ' ',
   title-overline? :: <boolean> = #f,
   title-midline? :: <boolean> = #f,
   title-underline? :: <boolean> = #f;
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

define caching parser title-midline-style (<token>)
   rule seq(opt(ascii-overline),
            choice(seq(opt-many(title-line-midline-style-til-nickname),
                       title-nickname-line-midline-style),
                   seq(many(title-line-midline-style),
                       nil(#f))),
            opt(ascii-underline))
      => tokens;
   slot title-content :: <title-word-sequence> = extract-title(tokens[1]);
   slot title-nickname :: false-or(<title-nickname-token>) =
      tokens[1][1] & tokens[1][1].title-nickname;
end;

define caching parser title-bare-style (<token>)
   rule seq(opt(ascii-overline),
            choice(seq(opt-many(title-line-bare-style-til-nickname),
                       title-nickname-line-bare-style),
                   seq(many(title-line-bare-style),
                       nil(#f))),
            ascii-underline)
      => tokens;
   slot title-content :: <title-word-sequence> = extract-title(tokens[1]);
   slot title-nickname :: false-or(<title-nickname-token>) =
      tokens[1][1] & tokens[1][1].title-nickname;
end;

//
// Title lines
//

define caching parser title-line-midline-style-til-nickname :: <title-line-midline-style-token>
   rule seq(not-next(title-nickname-line-midline-style), title-line-midline-style)
      => tokens;
   yield tokens[1];
end;

define caching parser title-line-bare-style-til-nickname :: <title-line-bare-style-token>
   rule seq(not-next(title-nickname-line-bare-style), title-line-bare-style)
      => tokens;
   yield tokens[1];
end;

define caching parser title-line-midline-style (<token>)
   rule seq(sol, ascii-midline, spaces, title-words, opt(ascii-midline), ls)
      => tokens;
   slot content :: <title-word-sequence> = tokens[3];
end;

define caching parser title-line-bare-style (<token>)
   rule seq(sol, title-words, ls) => tokens;
   slot content :: <title-word-sequence> = tokens[1];
end;

define caching parser title-nickname-line-midline-style (<token>)
   rule seq(sol, ascii-midline, spaces,
            opt-choice(seq(opt(title-words), ascii-midline, spaces),
                       seq(title-words-til-nickname)),
            title-nickname)
      => tokens;
   // tokens[3] will be one of
   // - #f
   // - #[ #f, ascii-midline, spaces ]
   // - #[ #[ title-words,... ], ascii-midline, spaces]
   // - #[ #[ title-words,... ] ]
   slot content :: <title-word-sequence> = (tokens[3] & tokens[3][0]) | #[];
   slot title-nickname :: <title-nickname-token> = tokens[4];
end;

define caching parser title-nickname-line-bare-style (<token>)
   rule seq(sol, opt(title-words-til-nickname), title-nickname) => tokens;
   slot content :: <title-word-sequence> = tokens[1] | #[];
   slot title-nickname :: <title-nickname-token> = tokens[2];
end;

//
// Title words
//

define caching parser title-words :: <title-word-sequence>
   rule many(seq(title-word, opt-spaces)) => tokens;
   yield collect-subelements(tokens, 0);
end;

define caching parser title-words-til-nickname :: <title-word-sequence>
   rule many(seq(not-next(title-nickname), title-word, opt-spaces)) => tokens;
   yield collect-subelements(tokens, 1);
end;

// exported
define caching parser title-nickname (<source-location-token>)
   rule seq(opn-brack-spc, nickname-word, spc-cls-brack, ls) => tokens;
   slot text :: <string> = tokens[1];
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

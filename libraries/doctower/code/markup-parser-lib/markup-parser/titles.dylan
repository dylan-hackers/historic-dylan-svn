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
   rule topic-or-section-title => token;
   yield token;
afterwards (context, token, value, start-pos, end-pos, fail: fail)
   unless (token.title-style ~= $section-style)
      fail(make(<parse-failure>))
   end unless;
end;
   
define caching parser section-title :: <topic-or-section-title-token>
   rule topic-or-section-title => token;
   yield token;
afterwards (context, token, value, start-pos, end-pos, fail: fail)
   unless (token.title-style = $section-style)
      fail(make(<parse-failure>))
   end unless;
end;

//
// Directive titles
//

// exported
define caching parser topic-directive-title (<source-location-token>)
   rule seq(directive-spec-intro, topic-directive-spec-text, colon, spaces,
            title-words-til-midline-nickname, opt-seq(ascii-midline, opt-spaces),
            choice(ls, title-nickname), opt(ascii-underline))
      => tokens;
   slot title-type :: <symbol> = tokens[1];
   slot title-content :: <title-word-sequence> = tokens[4];
   slot title-nickname :: false-or(<title-nickname-token>) =
      instance?(tokens[6], <title-nickname-token>) & tokens[6];
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

// exported
define caching parser section-directive-title (<source-location-token>)
   rule seq(directive-spec-intro, section-directive-spec-text, colon, spaces,
            title-words-til-midline-nickname, opt-seq(ascii-midline, opt-spaces),
            choice(ls, title-nickname), opt(ascii-underline))
      => tokens;
   slot title-type :: <symbol> = tokens[1];
   slot title-content :: <title-word-sequence> = tokens[4];
   slot title-nickname :: false-or(<title-nickname-token>) =
         instance?(tokens[6], <title-nickname-token>) & tokens[6];
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

//
// Title styles
//

// exported
define caching parser topic-or-section-title (<source-location-token>)
   rule choice(title-midline-style, title-bare-style) => token;
   slot title-style :: <topic-level-style> =
      make(<topic-level-style>, char: attr(title-line-char),
           over: token.title-overline?, mid: token.title-midline?,
           under: token.title-underline?);
   slot title-content :: <title-word-sequence> = token.title-content;
   slot title-nickname :: false-or(<title-nickname-token>) = token.title-nickname;
attributes
   title-line-char :: <character> = ' ';
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
   slot title-overline? :: <boolean> = tokens[0].true?;
   slot title-midline? :: <boolean> = #t;
   slot title-underline? :: <boolean> = tokens[2].true?;
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
   slot title-overline? :: <boolean> = tokens[0].true?;
   slot title-midline? :: <boolean> = #f;
   slot title-underline? :: <boolean> = #t;
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
   rule seq(not-next(ascii-underline), sol, ascii-midline, spaces,
            title-words-til-midline, opt(ascii-midline), ls)
      => tokens;
   slot content :: <title-word-sequence> = tokens[4];
end;

define caching parser title-line-bare-style (<token>)
   rule seq(not-next(ascii-underline), sol, title-words, ls) => tokens;
   slot content :: <title-word-sequence> = tokens[2];
end;

define caching parser title-nickname-line-midline-style (<token>)
   rule seq(not-next(ascii-underline), sol, ascii-midline, spaces,
            opt(title-words-til-midline-nickname),
            opt-seq(ascii-midline, spaces), title-nickname)
      => tokens;
   slot content :: <title-word-sequence> = tokens[4] | #[];
   slot title-nickname :: <title-nickname-token> = tokens[6];
end;

define caching parser title-nickname-line-bare-style (<token>)
   rule seq(not-next(ascii-underline), sol, opt(title-words-til-nickname),
            title-nickname) => tokens;
   slot content :: <title-word-sequence> = tokens[2] | #[];
   slot title-nickname :: <title-nickname-token> = tokens[3];
end;

//
// Title words
//

define caching parser title-words :: <title-word-sequence>
   rule many(seq(title-word, opt-spaces)) => tokens;
   yield collect-subelements(tokens, 0);
end;

define caching parser title-words-til-midline :: <title-word-sequence>
   rule many(seq(not-next(ascii-midline), title-word, opt-spaces)) => tokens;
   yield collect-subelements(tokens, 1);
end;

define caching parser title-words-til-nickname :: <title-word-sequence>
   rule many(seq(not-next(title-nickname), title-word, opt-spaces)) => tokens;
   yield collect-subelements(tokens, 1);
end;

define caching parser title-words-til-midline-nickname :: <title-word-sequence>
   rule many(seq(not-next(ascii-midline), not-next(title-nickname),
                 title-word, opt-spaces))
      => tokens;
   yield collect-subelements(tokens, 2);
end;

// exported
define caching parser title-nickname (<source-location-token>)
   rule seq(opn-brack-spc, nickname-word, spc-cls-brack, ls) => tokens;
   slot text :: <string> = tokens[1];
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

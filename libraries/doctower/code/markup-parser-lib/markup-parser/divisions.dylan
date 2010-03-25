module: markup-parser

//
// Types
//

define constant <topic-content-types> =
      type-union(<paragraph-directive-token>,
                 <link-directive-token>,
                 <links-directive-token>,
                 <word-directive-token>,
                 <division-directive-token>,
                 <titled-section-token>,
                 <titled-directive-section-token>,
                 <footnote-token>,
                 <division-content-types>);
                 
define constant <division-content-types> =
      type-union(<marginal-code-block-token>,
                 <marginal-verbatim-block-token>,
                 <figure-ref-line-token>,
                 <content-ref-line-token>,
                 <ditto-ref-line-token>,
                 <api-list-ref-line-token>,
                 <bracketed-raw-block-token>,
                 <table-token>,
                 <bullet-list-token>,
                 <numeric-list-token>,
                 <hyphenated-list-token>,
                 <phrase-list-token>,
                 <indented-content-directive-token>,
                 <paragraph-token>);

// These result in an error if actually used in type specifications.
// define constant <topic-content-sequence> =
//       limited(<sequence>, of: <topic-content-types>);
// 
// define constant <division-content-sequence> =
//       limited(<sequence>, of: <division-content-types>);

define constant <topic-content-sequence> = <sequence>;
define constant <division-content-sequence> = <sequence>;

//
// Markup
//

define caching parser markup-block :: <markup-content-token>
   rule seq(opt(indent), markup-content, opt(dedent), opt-many-spc-ls, not-next(char))
      => tokens;
   yield tokens[1];
afterwards (context, tokens, value, start-pos, end-pos, fail: fail)
   check-paired-indentation(tokens[0], tokens[2], fail)
end;

// exported
define caching parser markup-content (<source-location-token>)
   rule seq(opt(topic-content), opt-many(topic)) => tokens;
   slot default-topic-content :: <topic-content-sequence> = tokens[0] | #[];
   slot topics :: <sequence>
      /* of <directive-topic-token> or <titled-topic-token> */ =
      tokens[1] | #[]; 
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

//
// Topics
//

define caching parser topic :: type-union(<directive-topic-token>, <titled-topic-token>)
   rule choice(directive-topic, titled-topic) => token;
   yield token;
end parser;

// exported
define caching parser directive-topic (<source-location-token>)
   rule seq(directive-topic-title, opt(topic-content)) => tokens;
   slot topic-type :: <symbol> = tokens[0].title-type;
   slot topic-title :: <directive-topic-title-token> = tokens[0];
   slot topic-nickname :: false-or(<title-nickname-token>) =
      tokens[0].title-nickname;
   slot content :: <topic-content-sequence> = tokens[1] | #[];
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

// exported
define caching parser titled-topic (<source-location-token>)
   rule seq(topic-title, opt(topic-content)) => tokens;
   slot topic-title :: <topic-or-section-title-token> = tokens[0];
   slot topic-nickname :: false-or(<title-nickname-token>) =
      tokens[0].title-nickname;
   slot content :: <topic-content-sequence> = tokens[1] | #[];
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

define caching parser topic-content :: <topic-content-sequence>
   rule many(choice(section, footnote, division-content)) => tokens;
   yield integrate-sequences(tokens);
end;

//
// Sections
//

define caching parser section
      :: type-union(<titled-section-token>,
                    <paragraph-directive-token>,
                    <link-directive-token>,
                    <links-directive-token>,
                    <word-directive-token>,
                    <division-directive-token>,
                    <division-content-sequence>,
                    <titled-directive-section-token>)
   rule choice(directive-section, titled-directive-section, titled-section) 
      => token;
   yield token;
end;

// null-directive yields <division-content-sequence>
define caching parser directive-section
      :: type-union(<paragraph-directive-token>,
                    <link-directive-token>,
                    <links-directive-token>,
                    <word-directive-token>,
                    <division-directive-token>,
                    <division-content-sequence>)
   rule choice(paragraph-directive, link-directive, links-directive,
               word-directive, division-directive, null-directive)
      => token;
   yield token;
end;

// exported
define caching parser titled-section (<source-location-token>)
   rule seq(section-title, opt(division-content)) => tokens;
   slot section-title :: <topic-or-section-title-token> = tokens[0];
   slot section-nickname :: false-or(<title-nickname-token>) =
      tokens[0].title-nickname;
   slot content :: <division-content-sequence> = tokens[1] | #[];
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

// exported
// Not subclassed from <titled-section-token> because of the disjoint
// section-title slot.
define caching parser titled-directive-section (<source-location-token>)
   rule seq(directive-section-title, opt(division-content))
      => tokens;
   slot section-title :: <directive-section-title-token> = tokens[0];
   slot section-nickname :: false-or(<title-nickname-token>) =
      tokens[0].title-nickname;
   slot content :: <division-content-sequence> = tokens[1] | #[];
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

//
// Footnotes
//

// exported
define caching parser footnote (<source-location-token>)
   rule seq(opn-brack-spc, choice(number, ordinal), spc-cls-brack, colon,
            spaces, opt(division-content))
      => tokens;
   slot index :: type-union(<integer>, <string>) = tokens[1];
   slot content :: <division-content-sequence> = tokens[5] | #[];
afterwards (context, tokens, value, start-pos, end-pos)
   note-source-location(context, value)
end;

//
// Division content
//

// The choose removes #f (i.e. blank-lines) from the content blocks.
define caching parser division-content :: <division-content-sequence>
   rule many(choice(seq(nil(#f), indented-content),
                    seq(not-next(division-break), content-block)))
      => tokens;
   yield choose(true?, collect-subelements(tokens, 1));
end;

define caching parser division-break
   rule choice(topic, section, footnote);
end;

// The choose removes #f (i.e. blank-lines) from the content blocks.
define caching parser indented-content :: <division-content-sequence>
   rule seq(indent, many(content-block), dedent) => tokens;
   yield choose(true?, tokens[1]);
end;

define caching parser remainder-and-indented-content :: <division-content-sequence>
   rule seq(markup-words, ls, opt(indented-content)) => tokens;
   yield prepend-words(tokens[0], tokens[2]);
end;

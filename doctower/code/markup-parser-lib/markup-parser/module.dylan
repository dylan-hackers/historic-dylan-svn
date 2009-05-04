module: dylan-user
synopsis: This module parses Doxygen markup read from a stream.

define module markup-parser
   use common, exclude: { table, source-location };
   use parser-common, export: { source-location };
   use conditions;
   use configs;
   
   // from peg-parser
   use peg-parser, export: { <token>, *parser-trace* };
   // from string-extensions
   use character-type;
   // from wrapper-streams
   use replacing-stream;
   // from regular-expressions
   use regular-expressions, import: { regexp-replace };
   
   export
      parse-markup;
   
   export
      <markup-content-token>, <directive-topic-token>, <titled-topic-token>,
      <titled-section-token>, <footnote-token>,
      <topic-or-section-title-token>, <title-nickname-token>,
      <directive-topic-title-token>, <paragraph-directive-token>,
      <link-directive-token>, <links-directive-token>,
      <division-directive-token>, <indented-content-directive-token>,
      <marginal-code-block-token>, <marginal-verbatim-block-token>,
      <figure-ref-line-token>, <content-ref-line-token>,
      <ditto-ref-line-token>, <api-list-ref-line-token>,
      <bracketed-raw-block-token>, <table-token>, <bullet-list-token>,
      <bullet-list-item-token>, <numeric-list-token>,
      <numeric-list-item-token>, <hyphenated-list-token>,
      <hyphenated-list-item-token>, <phrase-list-token>,
      <phrase-list-item-token>, <paragraph-token>, <raw-line-token>,
      <image-ref-token>, <line-marker-ref-token>, <footnote-ref-token>,
      <synopsis-ref-token>, <quote-token>, <quote-spec-token>,
      <text-word-token>, <link-word-token>, <api-ref-token>,
      <bracketed-render-span-token>;
      
   export
      block-type, caption, close-quote, content, default-topic-content,
      directive-type, filename, index, item-label, link, links, list-start,
      list-type, open-quote, ordinal, postquoted-text, prequoted-text,
      quote-options, quote-spec, quoted-text, scale-factor, scale-type,
      section-nickname, section-title, text, title-content, title-style,
      title-text, topic-nickname, topic-title, topic-type, topics;
      
end module;

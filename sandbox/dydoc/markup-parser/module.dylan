module: dylan-user
synopsis: This module parses Doxygen markup read from a stream.

define module markup-parser
   use common, exclude: { table };
   use configs;
   
   // from peg-parser
   use peg-parser, export:
      {  <token>, <parse-failure>, parse-expected, failure-position,
         *parser-trace* };

   // from string-extensions
   use character-type, exclude: { case-insensitive-equal };
   
   export
      parse-markup;
   
   export
      <topic-directive-token>, <titled-topic-token>, <footnote-token>,
      <paragraph-token>, <marginal-code-block-token>,
      <marginal-verbatim-block-token>, <bullet-list-token>,
      <numeric-list-token>, <phrase-list-token>, <hyphenated-list-token>,
      <figure-ref-line-token>, <content-ref-line-token>,
      <ditto-ref-line-token>, <api-list-ref-line-token>,
      <bracketed-raw-block-token>, <bullet-list-first-item-token>,
      <bullet-list-item-token>, <numeric-list-first-item-token>,
      <numeric-list-item-token>, <hyphenated-list-item-token>,
      <phrase-list-item-token>, <raw-line-token>, <perc-scale-token>,
      <mult-scale-token>, <paragraph-directive-token>, <link-directive-token>,
      <links-directive-token>, <indented-directive-token>, <image-ref-token>,
      <marker-ref-token>, <bracketed-render-block-token>,
      <synopsis-ref-token>, <quote-token>, <ascii-line-token>;
      
   export
      ascii-line-char, ascii-midline?, ascii-overline?, ascii-underline?,
      block-type, caption, close-quote, content, directive-type, factor,
      filename, index, item-label, link, links, list-start, list-type,
      open-quote, quote-spec, quoted-content, postquoted-content,
      prequoted-content, scale-factor, topic-nickname, topic-title,
      topic-type;
      
end module;

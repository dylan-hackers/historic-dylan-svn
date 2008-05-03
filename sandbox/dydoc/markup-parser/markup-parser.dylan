module: markup-parser
synopsis: Parser initialization and overall control.


/// Synopsis: Tracks parser state and grammar variables.
define class <markup-context> (<object>)
   slot indent-stack :: <deque> = make(<deque>);
   slot end-quote-char :: false-or(<character>) = #f;
   slot end-bracket-spec :: false-or(<symbol>) = #f;
   slot ascii-line-char :: false-or(<character>) = #f;
   slot list-stack :: <deque> = make(<deque>);
end class;


// Synopsis: A way to note the file stream from which a token came.
define class <token-source> (<object>)
   slot token-source :: <file-stream>
end class;


/// Synopsis: Entry point into parsing.
/// Conditions: Throws <parse-failure> if stream has syntax error.
define method parse-markup (text :: <file-stream>) => (contents :: <sequence>)
   let context = make(<markup-context>);
   let markup-block-contents = parse-markup-block(text, context);
   log-object("Markup", markup-block-contents);
   visit-token-sources(markup-block-contents, rcurry(token-source-setter, text));
   markup-block-contents
end method;


/// Generic Function: visit-token-sources
/// Synopsis: Visits all <token>s that are also <token-source>s.
///
/// Arguments:
///   element     - The <token-source> to visit.
///   operation   - A <function> on 'element'.
/// Values:
///   result      - The result of 'operation'.

define collection-recursive slot-visitor visit-token-sources
   <topic-directive-token>,         content;
   <titled-topic-token>,            content;
   <footnote-token>,                content;
   <paragraph-token>,               content;
   <marginal-code-block-token>,     content;
   <marginal-verbatim-block-token>, content;
   <bullet-list-token>,             content;
   <numeric-list-token>,            content;
   <phrase-list-token>,             content;
   <hyphenated-list-token>,         content;
   <figure-ref-line-token>,         scale-factor;
   <content-ref-line-token>,        ;
   <ditto-ref-line-token>,          ;
   <api-list-ref-line-token>,       ;
   <bracketed-raw-block-token>,     content;
   <bullet-list-item-token>,        content;
   <numeric-list-first-item-token>, content;
   <numeric-list-item-token>,       content;
   <hyphenated-list-item-token>,    content, item-label;
   <phrase-list-item-token>,        content, item-label;
   <raw-line-token>,                ;
   <perc-scale-token>,              ;
   <mult-scale-token>,              ;
   <paragraph-directive-token>,     content;
   <link-directive-token>,          ;
   <links-directive-token>,         links;
   <indented-directive-token>,      content;
   <image-ref-token>,               ;
   <marker-ref-token>,              ;
   <bracketed-render-block-token>,  ;
   <synopsis-ref-token>,            ;
   <quote-token>,                   ;
   <ascii-line-token>,              ;
end slot-visitor;

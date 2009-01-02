module: internal-rep
synopsis: Classes comprising documentation.

/**
Synopsis: List of elements corresponding to the markup-words grammar.
   text-word         - <string> and <character>
   image-ref         - <inline-image>
   quote             - <xref>, <vi-xref>, <api-name>, <bold>, etc.
   bracketed-render-span   - <dita-content> or <html-content>
   api-ref           - Basically converts to an API qv.
   line-marker-ref   - An <xref> to a <ph-marker>.
   footnote-ref      - An <xref> to a <footnote>.
   synopsis-ref      - <conref> with the #"title" or #"shortdesc" style.
**/
define constant <markup-seq> = limited(<stretchy-vector>,
      of: type-union(<string>, <character>, <inline-image>, <html-content>,
                     <dita-content>, <conref>, <xref>, <api-name>,
                     <parm-name>, <term>, <term-style>, <code-phrase>,
                     <entity>, <cite>, <bold>, <italic>, <underline>,
                     <emphasis>, singleton(#f)));

/**                     
Synopsis: List of elements corresponding to content-block grammar.
   marginal-code-block  - <code-block>
   marginal-verbatim-block - <pre>
   figure-ref-line      - <fig>
   content-ref-line     - <conref> with the #"toc" style.
   ditto-ref-line       - <ditto-placeholder>
   api-list-ref-line    - <api-list-placeholder>
   bracketed-raw-block  - <code-block> or <pre>
   table                - <simple-table>
   bullet-list          - <unordered-list>
   numeric-list         - <ordered-list>
   hyphenated-list      - <one-line-defn-list>
   phrase-list          - <many-line-defn-list>
   paragraph            - <paragraph>
**/
define constant <content-seq> = limited(<stretchy-vector>,
   of: type-union(<code-block>, <pre>, <fig>, <conref>, <ditto-placeholder>,
                  <api-list-placeholder>, <simple-table>, <unordered-list>,
                  <ordered-list>, <defn-list>, <paragraph>, singleton(#f)));

define class <section> (<interm-element>)
   slot id :: false-or(<string>) = #f;
   slot title = make(<title-seq>);
   slot content = make(<content-seq>);

   slot title-source-loc :: <source-location>;
   slot id-source-loc :: <source-location>;
end class;

/**
In DITA, footnotes are done with the <fn> tag. In HTML, they are rendered at
the end of the topic before sub-topics.
**/
define class <footnote> (<interm-element>)
   slot index :: type-union(<character>, <integer>), init-keyword: #"index";
   slot content = make(<content-seq>);
end class;

define class <paragraph> (<interm-element>)
   slot content = make(<markup-seq>);
end class;

define class <note> (<interm-element>)
   slot content = make(<content-seq>);
end class;

define class <warning-note> (<note>)
end class;

define class <conref> (<interm-element>)
   slot target :: type-union(<topic>, <target-placeholder>),
      init-keyword: #"target";
   slot style :: one-of(#"title", #"shortdesc", #"toc"),
      init-keyword: #"style";
end class;

/// This will be rendered as an empty DITA <ph> or HTML anchor. Technically,
/// DITA <codeph> is a sub-class of <ph>, but I'm not using them at all the same.
/// Don't want to include content in the tag in case DITA processors won't consider
/// it as code in a code block.
define class <ph-marker> (<interm-element>)
   slot index :: type-union(<integer>, <character>), init-keyword: #"index";
end class;

define class <ordered-list> (<interm-element>)
   slot start :: type-union(<integer>, <character>), init-keyword: #"start";
   slot items :: <vector> /* of <content-seq> */;
end class;

define class <unordered-list> (<interm-element>)
   slot items :: <vector> /* of <content-seq> */;
end class;

define class <defn-list> (<interm-element>)
   slot items :: <array> /* 2-by-n, first col of <markup-seq>,
                            second col of <content-seq> */;
end class;

define class <one-line-defn-list> (<defn-list>)
end class;

define class <many-line-defn-list> (<defn-list>)
end class;

define class <fig> (<interm-element>)
   slot image-name :: <string>;
   slot abs-size :: false-or(<integer>) = #f;
   slot rel-size :: false-or(<integer>) = #f;
   slot title :: <string>;
end class;

define class <inline-image> (<interm-element>)
   slot image-name :: <string>, init-keyword: #"image";
   slot alt-text :: <string>, init-keyword: #"alt-text";
end class;

define class <pre> (<interm-element>)
   slot content = make(<stretchy-vector>) /* of <string>, <ph-marker> */;
end class;

define class <simple-table> (<interm-element>)
   slot headings :: <vector>;
   slot items :: <array>;
end class;

define class <code-block> (<pre>)
end class;

/// Mixin class for <defn-list> indicating an argument or value list.
define class <parm-list> (<object>)
end class;

define class <one-line-parm-list> (<one-line-defn-list>, <parm-list>)
end class;

define class <many-line-parm-list> (<many-line-defn-list>, <parm-list>)
end class;

define class <html-content> (<interm-element>)
   slot content :: <string>, init-keyword: #"content";
end class;

define class <dita-content> (<interm-element>)
   slot content :: <string>, init-keyword: #"content";
end class;

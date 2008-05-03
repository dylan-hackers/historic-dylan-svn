module: internal-rep
synopsis: Classes comprising documentation.

/// Synopsis: List of elements corresponding to the markup-words grammar.
///   text           - <string> and <character>
///   image-ref      - <inline-image>
///   quote          - <xref>, <toc-xref>, <api-name>, <bold>, etc.
///   bracketed-render-block  - <dita-content> or <html-content>
///   api-ref        - Basically converts to an API qv.
///   marker-ref     - An <xref> to a <footnote> or <ph-marker>.
///   synopsis-ref   - <conref> with the #"title" or #"shortdesc" style.
define constant <markup-deque> = limited(<deque>,
      of: type-union(<string>, <character>, <inline-image>, <html-content>,
                     <dita-content>, <conref>, <xref>, <toc-xref>, <api-name>,
                     <parm-name>, <term>, <term-style>, <code-phrase>, <entity>,
                     <cite>, <bold>, <italic>, <underline>, <emphasis>));

define class <section> (<interm-element>)
   slot id :: false-or(<string>);
   slot title = make(<title-deque>);
   slot content = make(<deque>);
end class;

define class <footnote> (<interm-element>)
   slot number;
   slot content = make(<deque>);
end class;

define class <paragraph> (<interm-element>)
   slot content = make(<markup-deque>);
end class;

define class <note> (<interm-element>)
   slot content;
end class;

define class <warning-note> (<note>)
end class;

define class <conref> (<interm-element>)
   slot target :: type-union(<topic>, <target-placeholder>);
   slot style :: one-of(#"title", #"shortdesc", #"toc");
end class;

/// This will be rendered as an empty DITA <ph> or HTML anchor. Technically,
/// DITA <codeph> is a sub-class of <ph>, but I'm not using them at all the same.
/// Don't want to include content in the tag in case DITA processors won't consider
/// it as code in a code block.
define class <ph-marker> (<interm-element>)
   slot index :: false-or(type-union(<integer>, <character>)),
         init-keyword: #"index";
end class;

define class <ordered-list> (<interm-element>)
   slot style :: one-of(#"num", #"alpha");
   slot start :: <integer> = 1;
   slot items;
end class;

define class <unordered-list> (<interm-element>)
   slot items;
end class;

define class <defn-list> (<interm-element>)
   slot items :: <array>;
end class;

define class <one-line-defn-list> (<defn-list>)
end class;

define class <two-line-defn-list> (<defn-list>)
end class;

define class <fig> (<interm-element>)
   slot image;
   slot abs-size :: false-or(<integer>);
   slot rel-size :: false-or(<integer>);
   slot title :: <string>;
end class;

define class <inline-image> (<interm-element>)
   slot image, init-keyword: #"image";
   slot alt-text :: <string>, init-keyword: #"alt-text";
end class;

define class <pre> (<interm-element>)
   slot content;
end class;

define class <simple-table> (<interm-element>)
   slot headings;
   slot items :: <array>;
end class;

define class <code-block> (<pre>)
end class;

define class <parm-list> (<defn-list>)
end class;

define class <html-content> (<interm-element>)
   slot content :: <string>, init-keyword: #"content";
end class;

define class <dita-content> (<interm-element>)
   slot content :: <string>, init-keyword: #"content";
end class;

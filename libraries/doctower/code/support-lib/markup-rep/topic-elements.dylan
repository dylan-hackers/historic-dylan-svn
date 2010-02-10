module: markup-rep
synopsis: Classes comprising API reference topics.

/**
Synopsis: List of elements corresponding to the title-words grammar.
   text-word   - <string> and <character>
   image-ref   - <inline-image>
   quote       - <bold>, etc., but not <xref>, <vi-xref>, <api-name>, or
                 <parm-name> which are introduced by qv and vi.
   bracketed-render-span   - <dita-content> or <html-content>
**/
define constant <title-seq> = limited(<stretchy-vector>,
      of: type-union(<string>, <character>, <inline-image>, <html-content>,
                     <dita-content>, <term>, <term-style>, <code-phrase>, <entity>,
                     <cite>, <bold>, <italic>, <underline>, <emphasis>,
                     singleton(#f)));

define method title-seq (#rest elements) => (seq :: <title-seq>)
   make-limited-seq(<title-seq>, elements)
end method;

                     
/**
Synopsis: List of elements corresponding to topic-content grammar.
   section           - <section>, <note>, or <warning-note>
   footnote          - <footnote>
   division-content  - As in <content-deque>
**/
define constant <topic-content-seq> = limited(<stretchy-vector>,
      of: type-union(<code-block>, <pre>, <fig>, <conref>, <ditto-placeholder>,
                     <api-list-placeholder>, <simple-table>, <unordered-list>,
                     <ordered-list>, <defn-list>, <paragraph>, <note>, <section>,
                     <footnote>, singleton(#f)));

define method topic-content-seq (#rest elements) => (seq :: <topic-content-seq>)
   make-limited-seq(<topic-content-seq>, elements)
end method;


define class <topic> (<markup-element>)
   // No placeholder needed for fixed-parent, because the parent is known from
   // topic nesting markup.
   slot fixed-parent :: false-or(<topic>) = #f;
   
   // parent is parent topic as indicated by "Section:" directive. If not false,
   // should end up being the same as fixed-parent (if fixed-parent is not false).
   // If not the same, different parent topics caught during topic arrangement.
   slot parent :: false-or(<topic-ref> /* to <topic> or <target-placeholder> */)
         = #f;
   
   slot id :: false-or(<string>) = #f,
         init-keyword: #"id";
         
   slot title :: <title-seq> = make(<title-seq>),
         init-keyword: #"title";
   slot shortdesc :: false-or(<paragraph>) = #f;

   // Main topic content, including sections but excluding templated sections
   // such as "Arguments" or "Superclasses".
   slot content :: <topic-content-seq> = make(<topic-content-seq>);
   
   slot title-source-loc :: <source-location> = $unknown-source-location,
         init-keyword: #"title-id-source-location";
   slot id-source-loc :: <source-location> = $unknown-source-location,
         init-keyword: #"title-id-source-location";
   
   slot footnotes :: <sequence> /* of <footnote> */
         = make(<stretchy-vector>);
   
   slot see-also :: <sequence> /* of <topic-ref> to <topic>, <url>, or <target-placeholder> */
         = make(<stretchy-vector>);
   slot relevant-to :: <sequence> /* of <topic-ref> to <topic> or <target-placeholder> */
         = make(<stretchy-vector>);
end class;

define class <ref-topic> (<topic>)
end class;

define class <con-topic> (<topic>)
end class;

// BUGFIX: Gwydion Dylan does not support abstract subclasses of concrete classes.
define /*abstract*/ class <api-doc> (<ref-topic>)
   slot implicit-topic? :: <boolean> = #f, init-keyword: #"implicit";
   slot fully-qualified-name :: false-or(<string>) = #f, 
         init-keyword: #"qualified-name";
   slot definitions-section :: false-or(<section>) = #f;
end class;

define method make (class == <api-doc>, #key topic-type) => (inst :: <api-doc>)
   select (topic-type)
      #"class" => make(<class-doc>);
      #"variable" => make(<variable-doc>);
      #"function" => make(<function-doc>);
      #"generic-function" => make(<generic-doc>);
      #"library" => make(<library-doc>);
      #"module" => make(<module-doc>);
      #"macro" => make(<macro-doc>);
   end select;
end method;

define class <library-doc> (<api-doc>)
   slot modules-section :: false-or(<section>) = #f;
end class;

define class <module-doc> (<api-doc>)
   slot names-section :: false-or(<section>) = #f;
end class;

define class <binding-doc> (<api-doc>)
end class;

define class <class-doc> (<binding-doc>)
   slot keywords-section :: false-or(<section>) = #f;
   slot slots-section :: false-or(<section>) = #f;
   slot modifiers-section :: false-or(<section>) = #f;
   slot supers-section :: false-or(<section>) = #f;
   slot subs-section :: false-or(<section>) = #f;
   slot funcs-on-section :: false-or(<section>) = #f;
   slot funcs-returning-section :: false-or(<section>) = #f;
end class;

define class <variable-doc> (<binding-doc>)
   slot type-section :: false-or(<section>) = #f;
   slot value-section :: false-or(<section>) = #f;
end class;

define class <macro-doc> (<binding-doc>)
   slot syntax-section :: false-or(<section>) = #f;
   slot args-section :: false-or(<section>) = #f;
   slot vals-section :: false-or(<section>) = #f;
end class;

define class <function-doc> (<binding-doc>)
   slot args-section :: false-or(<section>) = #f;
   slot vals-section :: false-or(<section>) = #f;
   slot conds-section :: false-or(<section>) = #f;
end class;

define class <generic-doc> (<function-doc>)
   slot modifiers-section :: false-or(<section>) = #f;
   
   // TODO: Is this redundant to the method's fixed-parent slot? Depends on if
   // methods are always children of their generic function.
   slot method-topics :: <sequence> /* of <function-doc> */
         = make(<stretchy-vector>);
end class;

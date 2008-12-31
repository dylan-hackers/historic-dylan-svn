module: internal-rep
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

define class <topic> (<interm-element>)
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
         
   slot title = make(<title-seq>);
   slot shortdesc :: false-or(<paragraph>) = #f;
   slot content = make(<topic-content-seq>);
   
   slot title-source-loc :: <source-location>;
   slot id-source-loc :: <source-location>;
   
   slot footnotes = make(<stretchy-vector>) /* of <footnote> */;
   
   slot see-also = make(<stretchy-vector>) /* of <topic-ref> to <topic>, <url>,
                                              or <target-placeholder> */;
   slot relevant-to = make(<stretchy-vector>) /* of <topic-ref> to <topic> or
                                                 <target-placeholder> */;

   // see-also-section is filled in later after all see-alsos are resolved.
   slot see-also-section :: false-or(<section>) = #f;
end class;

define class <ref-topic> (<topic>)
end class;

define class <con-topic> (<topic>)
end class;

define class <api-doc> (<ref-topic>)
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

define class <class-doc> (<api-doc>)
   slot export-section :: false-or(<section>) = #f;
   slot keywords-section :: false-or(<section>) = #f;
   slot slots-section :: false-or(<section>) = #f;
   slot modifiers-section :: false-or(<section>) = #f;
   slot supers-section :: false-or(<section>) = #f;
   slot subs-section :: false-or(<section>) = #f;
   slot funcs-on-section :: false-or(<section>) = #f;
   slot funcs-returning-section :: false-or(<section>) = #f;
end class;

define class <slot-doc> (<api-doc>)
   slot setter-name :: false-or(<string>);
   slot getter-name :: false-or(<string>);
   slot keyword-name :: false-or(<string>);
end class;

define class <variable-doc> (<api-doc>)
   slot export-section :: false-or(<section>) = #f;
   slot type-section :: false-or(<section>) = #f;
   slot value-section :: false-or(<section>) = #f;
end class;

define class <function-doc> (<api-doc>)
   slot export-section :: false-or(<section>) = #f;
   slot args-section :: false-or(<section>) = #f;
   slot vals-section :: false-or(<section>) = #f;
   slot conds-section :: false-or(<section>) = #f;
end class;

define class <generic-doc> (<function-doc>)
   slot modifiers-section :: false-or(<section>) = #f;
   
   // TODO: Is this redundant to the method's fixed-parent slot?
   slot method-topics = make(<stretchy-vector>) /* of <function-doc> */;
end class;

define class <library-doc> (<api-doc>)
   slot modules-section :: false-or(<section>) = #f;
end class;

define class <module-doc> (<api-doc>)
   slot export-section :: false-or(<section>) = #f;
   slot names-section :: false-or(<section>) = #f;
end class;

define class <macro-doc> (<api-doc>)
   slot export-section :: false-or(<section>) = #f;
   slot syntax-section :: false-or(<section>) = #f;
   slot args-section :: false-or(<section>) = #f;
   slot vals-section :: false-or(<section>) = #f;
end class;

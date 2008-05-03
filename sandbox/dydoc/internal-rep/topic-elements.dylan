module: internal-rep
synopsis: Classes comprising API reference topics.

/// Synopsis: List of elements corresponding to the title-words grammar.
///   text           - <string> and <character>
///   image-ref      - <inline-image>
///   quote          - <bold>, etc., but not <xref>, <toc-xref>, <api-name>,
///                    or <parm-name> which are introducted by qv and toc.
///   bracketed-render-block  - <dita-content> or <html-content>
define constant <title-deque> = limited(<deque>,
      of: type-union(<string>, <character>, <inline-image>, <html-content>,
                     <dita-content>, <term>, <term-style>, <code-phrase>, <entity>,
                     <cite>, <bold>, <italic>, <underline>, <emphasis>));

define class <topic> (<interm-element>)
   // No placeholder needed for fixed-parent, because the parent is known from
   // topic nesting markup.
   slot fixed-parent :: false-or(<topic>) = #f;
   // parent is parent topic as indicated by "Section:" directive. If not false,
   // should end up being the same as fixed-parent (if fixed-parent is not false).
   // If not, caught during placeholder resolution.
   slot parent :: false-or(type-union(<topic>, <target-placeholder>)) = #f;
   slot id :: false-or(<string>) = #f;
   slot title :: <title>;
   slot shortdesc :: false-or(<paragraph>) = #f;
   slot content :: false-or(<markup>) = #f;
   slot see-also = make(<deque>) /* of <topic>, <url>, or <target-placeholder> */;
   slot relevant-to = make(<deque>) /* of <topic> or <target-placeholder> */;
   // see-also-section is filled in later after all see-alsos are resolved.
   slot see-also-section :: false-or(<section>) = #f;
end class;

define class <title> (<interm-element>)
   slot content :: <sequence> = make(<deque>);
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
      otherwise => error("Unknown topic-type in make(<api-doc>)");
   end select;
end method;

define class <class-doc> (<api-doc>)
   slot export-section :: <section>;
   slot keywords-section :: <section>;
   slot modifiers-section :: <section>;
   slot supers-section :: <section>;
   slot subs-section :: <section>;
   slot funcs-on-section :: <section>;
   slot funcs-returning-section :: <section>;
end class;

/// Only used temporarily for comment blocks associated with a slot. Split up
/// into getter, setter, and init-keyword ASAP.
define class <slot-doc> (<api-doc>)
   slot setter-name :: false-or(<string>);
   slot getter-name :: false-or(<string>);
   slot keyword-name :: false-or(<string>);
end class;

define class <variable-doc> (<api-doc>)
   slot export-section :: <section>;
   slot type-section :: <section>;
   slot value-section :: <section>;
end class;

define class <function-doc> (<api-doc>)
   slot export-section :: <section>;
   slot args-section :: <section>;
   slot vals-section :: <section>;
   slot conds-section :: <section>;
end class;

define class <generic-doc> (<function-doc>)
   slot modifiers-section :: <section>;
   slot method-topics :: <sequence>;
end class;

define class <library-doc> (<api-doc>)
   slot modules-section :: <section>;
end class;

define class <module-doc> (<api-doc>)
   slot export-section :: <section>;
   slot names-section :: <section>;
end class;

define class <macro-doc> (<api-doc>)
   slot export-section :: <section>;
   slot syntax-section :: <section>;
   slot args-section :: <section>;
   slot vals-section :: <section>;
end class;

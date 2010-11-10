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
      of: type-union(<string>, <character>, <inline-image>, <html-content>, <cite>,
                     <dita-content>, <api/parm-name>, <term>, <term-style>,
                     <code-phrase>, <entity>, <bold>, <italic>, <underline>,
                     <emphasis>, singleton(#f)));

define method title-seq (#rest elements) => (seq :: <title-seq>)
   make-limited-seq(<title-seq>, elements)
end method;

define method \= (title1 :: <title-seq>, title2 :: <title-seq>)
=> (equal? :: <boolean>)
   title1.stringify-title = title2.stringify-title
end method;

                     
/**
Synopsis: List of elements corresponding to topic-content grammar.
   section           - <section>, <note>, or <warning-note>
   footnote          - <footnote>
   division-content  - As in <content-seq>
**/
define constant <topic-content-seq> = limited(<stretchy-vector>,
      of: type-union(<code-block>, <pre>, <fig>, <ditto-placeholder>,
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
   
   // Parent topic as indicated by "Parent Topic:" directive. If not false,
   // should end up being the same as fixed-parent (if fixed-parent is not
   // false). If not the same, different parent topics caught during topic
   // arrangement.
   slot parent :: false-or(<topic-ref> /* to <topic> or <target-placeholder> */)
         = #f;
   
   slot id :: false-or(<string>) = #f,
         init-keyword: #"id";
   slot topic-type :: <symbol> = #"topic",
         init-keyword: #"topic-type";
   slot generated-topic? :: <boolean> = #f,
         init-keyword: #"generated";
         
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
   
   slot related-links :: <sequence> /* of <topic-ref> to <topic>, <url>, or <target-placeholder> */
         = make(<stretchy-vector>);
   slot relevant-to :: <sequence> /* of <topic-ref> to <topic> or <target-placeholder> */
         = make(<stretchy-vector>);
end class;

define method make
   (class == <topic>, #rest keys, #key topic-type = #"topic")
=> (inst :: <topic>)
   let subclass = 
         select (topic-type)
            #"class" => <class-doc>;
            #"constant" => <variable-doc>;
            #"variable" => <variable-doc>;
            #"function" => <function-doc>;
            #"method" => <function-doc>;
            #"generic-function" => <function-doc>;
            #"library" => <library-doc>;
            #"module" => <module-doc>;
            #"macro" => <macro-doc>;
            #"unbound" => <unbound-doc>;
            #"topic" => <con-topic>;
            #"catalog" => <catalog-topic>;
         end select;
   apply(make, subclass, keys);
end method;

define function printed-topic-type (topic-type :: <symbol>) => (string :: <string>)
   select (topic-type)
      #"topic" => "topic";
      #"generic-function" => "generic function topic";
      otherwise => concatenate(as(<string>, topic-type), " topic");
   end select
end function;

define class <con-topic> (<topic>)
end class;

define class <ref-topic> (<topic>)
end class;

define class <catalog-topic> (<ref-topic>)
   keyword topic-type: = #"catalog";
end class;

define class <api-doc> (<ref-topic>)
   // The fully qualified name, either automatically generated or supplied by
   // author and standardized.
   slot fully-qualified-name :: false-or(<string>) = #f, 
         init-keyword: #"qualified-name";
   slot fully-qualified-name-source-loc :: <source-location> = $unknown-source-location,
         init-keyword: #"qualified-name-source-location";
   
   // True if this API actually exists in code; false if it is conceptual and
   // provided by author. All automatically generated API topics are existent,
   // and authored topics that correspond to generated topics are also made
   // existent during topic merging.
   slot existent-api? :: <boolean> = #f,
         init-keyword: #"existent-api";
   
   // If the topic is an existent API, the enclosing namespace
   // (e.g. "Dylan:Dylan").
   slot canonical-namespace :: false-or(<string>) = #f,
         init-keyword: #"namespace";

   // Title or titles used in each enclosing namespace. Keys are <string>,
   // elements are sequence of <title-seq>. If the topic is an existent API,
   // there will be an element at 'canonical-namespace' that includes 'title'.
   // These titles are used when constructing API lists for a specific namespace.
   slot titles-in-namespace :: <table> = make(<case-insensitive-string-table>);
         
   slot definitions-section :: false-or(<section>) = #f;
end class;

define class <library-doc> (<api-doc>)
   slot modules-section :: false-or(<section>) = #f;
   keyword topic-type: = #"library";
end class;

define class <module-doc> (<api-doc>)
   slot bindings-section :: false-or(<section>) = #f;
   keyword topic-type: = #"module";
end class;

define class <unbound-doc> (<api-doc>)
   keyword topic-type: = #"unbound";
end class;

define class <class-doc> (<api-doc>)
   slot adjectives-section :: false-or(<section>) = #f;
   slot keywords-section :: false-or(<section>) = #f;
   slot conds-section :: false-or(<section>) = #f;
   slot inheritables-section :: false-or(<section>) = #f;
   slot supers-section :: false-or(<section>) = #f;
   slot subs-section :: false-or(<section>) = #f;
   slot funcs-on-section :: false-or(<section>) = #f;
   slot funcs-returning-section :: false-or(<section>) = #f;
   keyword topic-type: = #"class";
end class;

define class <variable-doc> (<api-doc>)
   slot adjectives-section :: false-or(<section>) = #f;
   slot value-section :: false-or(<section>) = #f;
   keyword topic-type: = #"variable";
end class;

define class <macro-doc> (<api-doc>)
   slot syntax-section :: false-or(<section>) = #f;
   slot args-section :: false-or(<section>) = #f;
   slot vals-section :: false-or(<section>) = #f;
   keyword topic-type: = #"macro";
end class;

define class <function-doc> (<api-doc>)
   slot adjectives-section :: false-or(<section>) = #f;
   slot args-section :: false-or(<section>) = #f;
   slot vals-section :: false-or(<section>) = #f;
   slot conds-section :: false-or(<section>) = #f;
   keyword topic-type: = #"function";
end class;

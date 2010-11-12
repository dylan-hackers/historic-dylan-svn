module: markup-rep
synopsis: Contains slot visitor functions.


/// Generic Function: visit-target-placeholders
/// Synopsis: Visits a <topic> and its nested elements that can contain
/// user-specified <target-placeholder> objects.
///
/// Arguments:
///   element     - The <markup-element> or collection to visit.
///   operation   - A <function> on 'element'. The setter is passed a setter:
///                 [api] argument and the 'keys' argument.
///   #rest keys  - A set of keys passed to 'operation'.

define collection-recursive slot-visitor visit-target-placeholders
   // Topic elements
   <topic>,                content, shortdesc, parent, footnotes, related-links,
                           relevant-to;
   <api-doc>,              declarations-section;
   <library-doc>,          modules-section;
   <module-doc>,           bindings-section;
   <class-doc>,            adjectives-section, keywords-section, conds-section,
                           inheritables-section, supers-section, subs-section,
                           funcs-on-section, funcs-returning-section;
   <function-doc>,         adjectives-section, args-section, vals-section, conds-section;
   <variable-doc>,         adjectives-section, value-section;
   <macro-doc>,            syntax-section, args-section, vals-section;
   <placeholder-doc>,      ;
   <unbound-doc>,          ;

   // Body elements
   <conref>,               target;
   <defn-list>,            items;
   <footnote>,             content;
   <note>,                 content;
   <ordered-list>,         items;
   <unordered-list>,       items;
   <paragraph>,            content;
   <section>,              title, content;
   <simple-table>,         headings, items;

   // Quote elements
   <api/parm-name>,        text;
   <bold>,                 text;
   <cite>,                 text;
   <code-phrase>,          text;
   <emphasis>,             text;
   <italic>,               text;
   <term-style>,           text;
   <term>,                 text;
   <underline>,            text;
   <xref>,                 target, text;
   
   // Placeholders
   <ditto-placeholder>,    target;
   <target-placeholder>,   ;
   <topic-ref>,            target;
   
   // Cut recursion
   <string>,               ;
end slot-visitor;


/// Generic function: visit-content-placeholders
/// Synopsis: Visit a <topic> and its nested elements that can contain
/// <api-list-placeholder> and <ditto-placeholder> objects.
///
/// Arguments:
///   element     - The <markup-element> or collection to visit.
///   operation   - A <function> on 'element'. The setter is passed a 'setter:'
///                 [api] argument and the 'keys' argument.
///   #rest keys  - A set of keys passed to 'operation'.

define collection-recursive slot-visitor visit-content-placeholders
   // Topic elements
   <topic>,                content, footnotes;
   <api-doc>,              declarations-section;
   <library-doc>,          modules-section;
   <module-doc>,           bindings-section;
   <class-doc>,            adjectives-section, keywords-section, conds-section,
                           inheritables-section, supers-section, subs-section,
                           funcs-on-section, funcs-returning-section;
   <function-doc>,         adjectives-section, args-section, vals-section, conds-section;
   <variable-doc>,         adjectives-section, value-section;
   <macro-doc>,            syntax-section, args-section, vals-section;
   <placeholder-doc>,      ;
   <unbound-doc>,          ;
   
   // Body elements
   <defn-list>,            items;
   <footnote>,             content;
   <note>,                 content;
   <ordered-list>,         items;
   <section>,              content;
   <simple-table>,         items;
   <unordered-list>,       items;
   
   // Placeholders
   <api-list-placeholder>  ;
   <ditto-placeholder>     ;
   
   // Cut recursion
   <string>                ;
end slot-visitor;


/// Generic function: visit-targets
/// Synopsis: Visit a <topic> and its nested elements that can contain <xref> or
/// <conref> targets.
///
/// These targets are <topic>, <section>, <footnote>, and <ph-marker>.
///
/// Arguments:
///   element     - The <markup-element> or collection to visit.
///   operation   - A <function> on 'element'. The setter is passed a 'setter:'
///                 [api] argument and the 'keys' argument.
///   #rest keys  - A set of keys passed to 'operation'.

define collection-recursive slot-visitor visit-targets
   // Topic elements
   <topic>,           content, footnotes;
   <api-doc>,         declarations-section;
   <library-doc>,     modules-section;
   <module-doc>,      bindings-section;
   <class-doc>,       adjectives-section, keywords-section, conds-section,
                      inheritables-section, supers-section, subs-section,
                      funcs-on-section, funcs-returning-section;
   <function-doc>,    adjectives-section, args-section, vals-section, conds-section;
   <variable-doc>,    adjectives-section, value-section;
   <macro-doc>,       syntax-section, args-section, vals-section;
   <placeholder-doc>, ;
   <unbound-doc>,     ;

   // Body elements
   <section>,         content;
   <footnote>,        content;
   <note>,            content;
   <ordered-list>,    items;
   <unordered-list>,  items;
   <defn-list>,       items;
   <simple-table>,    items;
   <ph-marker>,       ;
   <pre>,             content;
   
   // Cut recursion
   <string>,          ;
end slot-visitor;

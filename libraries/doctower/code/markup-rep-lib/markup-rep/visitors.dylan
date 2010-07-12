module: markup-rep
synopsis: Contains slot visitor functions.


/// Generic Function: visit-topic-placeholders
/// Synopsis: Visits a <topic> and its nested elements that can contain
/// user-specified <target-placeholder> objects referring to topics. Does not
/// include general <xref> objects.
///
/// Arguments:
///   element     - The <markup-element> to visit.
///   operation   - A <function> on 'element'. The setter is passed a setter:
///                 argument and the 'keys' argument.
///   #rest keys  - A set of keys passed to 'operation'.

define collection-recursive slot-visitor visit-topic-placeholders
   // Topic elements
   <topic>,                content, shortdesc, parent, footnotes, see-also, relevant-to;
   <library-doc>,          content, shortdesc, parent, footnotes, see-also, relevant-to,
                           definitions-section, modules-section;
   <module-doc>,           content, shortdesc, parent, footnotes, see-also, relevant-to,
                           definitions-section, bindings-section;
   <class-doc>,            content, shortdesc, parent, footnotes, see-also, relevant-to,
                           definitions-section, adjectives-section, keywords-section,
                           conds-section, inheritables-section, supers-section, 
                           subs-section, funcs-on-section, funcs-returning-section;
   <function-doc>,         content, shortdesc, parent, footnotes, see-also, relevant-to,
                           definitions-section, adjectives-section, args-section,
                           vals-section, conds-section;
   <generic-doc>,          content, shortdesc, parent, footnotes, see-also, relevant-to,
                           definitions-section, adjectives-section, args-section,
                           vals-section, conds-section, methods-section;
   <variable-doc>,         content, shortdesc, parent, footnotes, see-also, relevant-to,
                           definitions-section, adjectives-section, value-section;
   <macro-doc>,            content, shortdesc, parent, footnotes, see-also, relevant-to,
                           definitions-section, syntax-section, args-section, vals-section;
   <unbound-doc>,          content, shortdesc, parent, footnotes, see-also, relevant-to,
                           definitions-section;

   // Body elements
   <conref>,               target;
   <defn-list>,            items;
   <footnote>,             content;
   <ordered-list>,         items;
   <paragraph>,            content;
   <section>,              title, content;
   <simple-table>,         headings, items;
   <unordered-list>,       items;

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
   <vi-xref>,              target;
   // <xref> not included because it can contain non-topic placeholders.
   
   // Placeholders
   <ditto-placeholder>,    target;
   <target-placeholder>,   ;
   <toc-placeholder>,      target;
   <topic-ref>,            target;
end slot-visitor;


/// Generic Function: visit-vi-xrefs
/// Synopsis: Visits a <topic> and its nested elements that can contain <vi-xref>
/// objects.
///
/// Arguments:
///   element     - The <markup-element> to visit.
///   operation   - A <function> on 'element'. The setter is passed a setter:
///                 argument and the 'keys' argument.
///   #rest keys  - A set of keys passed to 'operation'.

define collection-recursive slot-visitor visit-vi-xrefs
   // Topic elements
   <topic>,           content, shortdesc, footnotes;
   <library-doc>,     content, shortdesc, footnotes, definitions-section,
                      modules-section;
   <module-doc>,      content, shortdesc, footnotes, definitions-section,
                      bindings-section;
   <class-doc>,       content, shortdesc, footnotes, definitions-section,
                      adjectives-section, keywords-section, conds-section,
                      inheritables-section, supers-section, subs-section,
                      funcs-on-section, funcs-returning-section;
   <function-doc>,    content, shortdesc, footnotes, definitions-section,
                      adjectives-section, args-section, vals-section, conds-section;
   <generic-doc>,     content, shortdesc, footnotes, definitions-section,
                      adjectives-section, args-section, vals-section, conds-section,
                      methods-section;
   <variable-doc>,    content, shortdesc, footnotes, definitions-section,
                      adjectives-section, value-section;
   <macro-doc>,       content, shortdesc, footnotes, definitions-section,
                      syntax-section, args-section, vals-section;
   <unbound-doc>,     content, shortdesc, footnotes, definitions-section;

   // Body elements
   <defn-list>,       items;
   <footnote>,        content;
   <note>,            content;
   <ordered-list>,    items;
   <paragraph>,       content;
   <section>,         content;
   <simple-table>,    headings, items;
   <unordered-list>,  items;
   
   // Quote elements
   <api/parm-name>,   text;
   <bold>,            text;
   <cite>,            text;
   <code-phrase>,     text;
   <emphasis>,        text;
   <italic>,          text;
   <term-style>,      text;
   <term>,            text;
   <vi-xref>,         ;
   <underline>,       text;
end slot-visitor;


/// Generic function: visit-content-placeholders
/// Synopsis: Visit a <topic> and its nested elements that can contain
/// <api-list-placeholder>, <ditto-placeholder>, and <toc-placeholder> objects.
///
/// Arguments:
///   element     - The <markup-element> to visit.
///   operation   - A <function> on 'element'. The setter is passed a setter:
///                 argument and the 'keys' argument.
///   #rest keys  - A set of keys passed to 'operation'.

define collection-recursive slot-visitor visit-content-placeholders
   // Topic elements
   <topic>,                content, footnotes;
   <library-doc>,          content, footnotes, definitions-section, modules-section;
   <module-doc>,           content, footnotes, definitions-section, bindings-section;
   <class-doc>,            content, footnotes, definitions-section, adjectives-section,
                           keywords-section, conds-section, inheritables-section,
                           supers-section, subs-section, funcs-on-section,
                           funcs-returning-section;
   <function-doc>,         content, footnotes, definitions-section, adjectives-section,
                           args-section, vals-section, conds-section;
   <generic-doc>,          content, footnotes, definitions-section, adjectives-section,
                           args-section, vals-section, conds-section, methods-section;
   <variable-doc>,         content, footnotes, definitions-section, adjectives-section,
                           value-section;
   <macro-doc>,            content, footnotes, definitions-section, syntax-section,
                           args-section, vals-section;
   <unbound-doc>,          content, footnotes, definitions-section;
   
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
   <toc-placeholder>       ;
end slot-visitor;

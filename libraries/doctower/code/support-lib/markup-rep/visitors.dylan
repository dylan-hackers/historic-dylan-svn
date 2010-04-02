module: markup-rep
synopsis: Contains slot visitor functions.


/// Generic Function: visit-placeholders
/// Synopsis: Visits a <topic> and its nested elements that can contain
/// user-specified <target-placeholder> objects.
///
/// Arguments:
///   element     - The <markup-element> to visit.
///   operation   - A <function> on 'element'. The setter is passed a setter:
///                 argument and the 'keys' argument.
///   #rest keys  - A set of keys passed to 'operation'.

define collection-recursive slot-visitor visit-placeholders
   <bold>,                 text;
   <cite>,                 text;
   <class-doc>,            content, shortdesc, parent, footnotes, see-also, relevant-to,
                           keywords-section, conds-section;
   <code-phrase>,          text;
   <conref>,               target;
   <defn-list>,            items;
   <ditto-placeholder>,    target;
   <emphasis>,             text;
   <footnote>,             content;
   <function-doc>,         content, shortdesc, parent, footnotes, see-also, relevant-to,
                           args-section, vals-section, conds-section;
   <italic>,               text;
   <macro-doc>,            content, shortdesc, parent, footnotes, see-also, relevant-to,
                           args-section, vals-section;
   <ordered-list>,         items;
   <paragraph>,            content;
   <section>,              title, content;
   <simple-table>,         headings, items;
   <target-placeholder>,   ;
   <term-style>,           text;
   <term>,                 text;
   <vi-xref>,              target;
   <topic>,                content, shortdesc, parent, footnotes, see-also, relevant-to;
   <topic-ref>,            target;
   <underline>,            text;
   <unordered-list>,       items;
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
   <bold>,            text;
   <cite>,            text;
   <class-doc>,       content, shortdesc, footnotes, keywords-section, conds-section;
   <code-phrase>,     text;
   <defn-list>,       items;
   <emphasis>,        text;
   <footnote>,        content;
   <function-doc>,    content, shortdesc, footnotes, args-section, vals-section,
                      conds-section;
   <italic>,          text;
   <macro-doc>,       content, shortdesc, footnotes, args-section, vals-section;
   <note>,            content;
   <ordered-list>,    items;
   <paragraph>,       content;
   <section>,         content;
   <simple-table>,    headings, items;
   <term-style>,      text;
   <term>,            text;
   <vi-xref>,         ;
   <topic>,           content, shortdesc, footnotes;
   <underline>,       text;
   <unordered-list>,  items;
end slot-visitor;

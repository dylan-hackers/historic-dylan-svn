module: topic-resolver

/// Generic Function: visit-placeholders
/// Synopsis: Visits a <topic> and its nested elements that can contain
/// <target-placeholder> objects.
///
/// Arguments:
///   element     - The <interm-element> to visit.
///   operation   - A <function> on 'element'.
/// Values:
///   result      - The result of 'operation'.

define collection-recursive slot-visitor visit-placeholders
   <bold>,	               text;
   <cite>,	               text;
   <class-doc>,	         content, shortdesc, parent, see-also, relevant-to,
                           keywords-section;
   <code-phrase>,	         text;
   <conref>,	            target;
   <defn-list>,	         items;
   <ditto-placeholder>,	   target;
   <emphasis>,	            text;
   <footnote>,	            content;
   <function-doc>,	      content, shortdesc, parent, see-also, relevant-to,
                           args-section, vals-section, conds-section;
   <italic>,	            text;
   <macro-doc>,	         content, shortdesc, parent, see-also, relevant-to,
                           args-section, vals-section;
   <ordered-list>,	      items;
   <paragraph>,	         content;
   <section>,	            title, content;
   <simple-table>,	      headings, items;
   <target-placeholder>,   ;
   <term-style>,	         text;
   <term>,	               text;
   <toc-xref>,	            target;
   <topic>,	               content, shortdesc, parent, see-also, relevant-to;
   <underline>,	         text;
   <unordered-list>,	      items;
end slot-visitor;


/// Generic Function: visit-toc-xrefs
/// Synopsis: Visits a <topic> and its nested elements that can contain <toc-xref>
/// objects.
///
/// Arguments:
///   element     - The <interm-element> to visit.
///   operation   - A <function> on 'element'.
/// Values:
///   result      - The result of 'operation'.

define collection-recursive slot-visitor visit-toc-xrefs
   <bold>,	          text;
   <cite>,            text;
   <class-doc>,	    content, shortdesc, keywords-section;
   <code-phrase>,	    text;
   <defn-list>,	    items;
   <emphasis>,	       text;
   <footnote>,	       content;
   <function-doc>,	 content, shortdesc, args-section, vals-section, conds-section;
   <italic>,	       text;
   <macro-doc>,       content, shortdesc, args-section, vals-section;
   <note>,	          content;
   <ordered-list>,	 items;
   <paragraph>,       content;
   <section>,         content;
   <simple-table>,	 headings, items;
   <term-style>,	    text;
   <term>,	          text;
   <toc-xref>,	       ;
   <topic>,           content, shortdesc;
   <underline>,	    text;
   <unordered-list>,	 items;
end slot-visitor;

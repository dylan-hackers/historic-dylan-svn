module: dylan-user
synopsis: Internal representation of documentation.

define module markup-rep
   use common;
   use ordered-tree;
   
   // from system
   use locators, import: { <url> }, export: all;

   export
      <api-doc>, <api-list-placeholder>, <api-name>, <bold>, <cite>,
      <binding-doc>, <class-doc>, <code-block>, <code-phrase>, <con-topic>,
      <conref>, <content-seq>, <defn-list>, <dita-content>, <ditto-placeholder>,
      <emphasis>, <entity>, <fig>, <footnote>, <footnote-placeholder>,
      <function-doc>, <generic-doc>, <html-content>, <inline-image>,
      <markup-element>, <italic>, <library-doc>, <macro-doc>,
      <many-line-defn-list>, <line-marker-placeholder>, <markup-seq>,
      <module-doc>, <note>, <one-line-defn-list>, <ordered-list>, <paragraph>,
      <parm-list>, <parm-name>, <ph-marker>, <pre>, <ref-topic>, <section>,
      <simple-table>, <target-placeholder>, <term-style>, <term>, <title-seq>,
      <toc-placeholder>, <topic-ref>, <topic>, <topic-content-seq>, <underline>,
      <unordered-list>, <variable-doc>, <vi-xref>, <warning-note>, <xref>;

   export
      content-seq, markup-seq, title-seq, topic-content-seq;

   export
      abs-size, abs-size-setter, args-section, args-section-setter,
      conds-section, conds-section-setter, content, content-setter,
      definitions-section, definitions-section-setter, footnotes,
      footnotes-setter, fully-qualified-name, fully-qualified-name-setter,
      headings, headings-setter, id, id-setter, id-source-loc,
      id-source-loc-setter, image-name, image-name-setter, index, index-setter,
      items, items-setter, keywords-section, keywords-section-setter,
      method-topics, modules-section, modules-section-setter, names-section,
      names-section-setter, parent, parent-setter, rel-size, rel-size-setter,
      relevant-to, relevant-to-setter, see-also, see-also-setter, shortdesc,
      shortdesc-setter, target, target-setter, text, text-setter, title,
      title-setter, title-source-loc, title-source-loc-setter, vals-section,
      vals-section-setter;

   export
      <topic-level-style>, stringify-title;
      
end module;
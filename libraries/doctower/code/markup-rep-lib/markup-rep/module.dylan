module: dylan-user
synopsis: Internal representation of documentation.

define module markup-rep
   use common;
   use ordered-tree;
   use markup-parser, import: { <topic-level-style> };
   
   // from system
   use locators, import: { <url> }, export: all;

   export
      <api-doc>, <api/parm-name>, <binding-doc>, <bold>, <catalog-topic>, <cite>,
      <unbound-doc>, <class-doc>, <code-block>, <code-phrase>, <con-topic>,
      <conref>, <content-seq>, <defn-list>, <dita-content>, <ditto-placeholder>,
      <emphasis>, <entity>, <fig>, <footnote>, <footnote-placeholder>,
      <function-doc>, <html-content>, <inline-image>, <markup-element>,
      <italic>, <library-doc>, <macro-doc>, <many-line-defn-list>,
      <many-line-parm-list>, <line-marker-placeholder>, <markup-seq>,
      <module-doc>, <note>, <one-line-defn-list>, <one-line-parm-list>,
      <ordered-list>, <paragraph>, <parm-list>, <ph-marker>, <placeholder-doc>,
      <pre>, <ref-topic>, <section>, <simple-table>, <target-placeholder>,
      <term-style>, <term>, <title-seq>, <topic-ref>, <topic>,
      <topic-content-seq>, <underline>, <unordered-list>, <variable-doc>,
      <vi-xref>, <warning-note>, <xref>;

   export
      content-seq, markup-seq, title-seq, topic-content-seq;

   export
      abs-size, abs-size-setter, adjectives-section, adjectives-section-setter,
      api-xrefs, api-xrefs-setter, args-section, args-section-setter,
      bindings-section, bindings-section-setter, canonical-namespace,
      canonical-namespace-setter, canonical-namespace-source-loc,
      canonical-namespace-source-loc-setter, code, conds-section,
      conds-section-setter, content, content-setter, declarations-section,
      declarations-section-setter, existent-api?, existent-api?-setter,
      fixed-parent, fixed-parent-setter, footnotes, footnotes-setter,
      fully-qualified-name, fully-qualified-name-setter, funcs-on-section,
      funcs-on-section-setter, funcs-returning-section,
      funcs-returning-section-setter, generated-topic?, headings,
      headings-setter, id, id-setter, id-source-loc, id-source-loc-setter,
      image-name, image-name-setter, index, index-setter, inheritables-section,
      inheritables-section-setter, items, items-setter, keywords-section,
      keywords-section-setter, modules-section, modules-section-setter,
      names-in-namespace, parent, parent-setter, qualified-scope-name,
      qualified-scope-name-setter, rel-size, rel-size-setter, relevant-to,
      relevant-to-setter, related-links, related-links-setter, shortdesc,
      shortdesc-setter, style, subs-section, subs-section-setter,
      supers-section, supers-section-setter, syntax-section,
      syntax-section-setter, target, target-setter, target-from-text?, text,
      text-setter, title, title-setter, title-source-loc,
      title-source-loc-setter, topic-type, vals-section, vals-section-setter,
      value-section, value-section-setter;

   export
      stringify-title, stringify-markup, printed-topic-type,
      visit-target-placeholders, visit-content-placeholders, visit-targets;
      
end module;

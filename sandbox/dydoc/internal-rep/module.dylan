module: dylan-user
synopsis: Internal representation of documentation.

define module internal-rep
   use common;
   use ordered-tree;
   
   // from system
   use locators, import: { <url> }, export: all;

   export
      <api-doc>, <api-list-placeholder>, <api-name>, <bold>, <cite>,
      <class-doc>, <code-block>, <code-phrase>, <con-topic>, <conref>,
      <defn-list>, <dita-content>, <ditto-placeholder>, <emphasis>, <entity>,
      <fig>, <footnote>, <function-doc>, <generic-doc>, <html-content>,
      <inline-image>, <interm-element>, <italic>, <library-doc>, <macro-doc>,
      <module-doc>, <note>, <one-line-defn-list>, <ordered-list>, <paragraph>,
      <parm-list>, <parm-name>, <ph-marker>, <pre>, <ref-topic>, <section>,
      <simple-table>, <slot-doc>, <target-placeholder>, <term-style>, <term>,
      <title>, <toc-xref>, <topic-ref>, <topic>, <two-line-defn-list>,
      <underline>, <unordered-list>, <variable-doc>, <warning-note>, <xref>;

   export
      args-section, args-section-setter, conds-section, conds-section-setter,
      content, content-setter, element-owner, element-owner-setter,
      element-source, headings, headings-setter, id, id-setter, items,
      items-setter, keywords-section, keywords-section-setter, parent,
      parent-setter, relevant-to, see-also, see-also-section,
      see-also-section-setter, shortdesc, shortdesc-setter, target,
      target-setter, text, text-setter, title, title-setter, vals-section,
      vals-section-setter;

   export
      <topic-level-style>, stringify-title;
      
end module;
module: dylan-user

define module conditions
   use common-imports;
   
   export
      <skip-error-restart>, error-condition,
      <user-visible-warning>, <user-visible-error>, error-code, error-location;
   
   export
      ambiguous-api-in-topics,
      ambiguous-title-in-link,
      api-not-found-in-code,
      bad-syntax-in-toc-file,
      circular-class-inheritance,
      circular-definition,
      conflicting-bindings-in-module,
      conflicting-libraries-in-filesets,
      conflicting-locations-in-tree,
      conflicting-modules-in-library,
      doc-comment-on-binding-alias,
      doc-comment-on-virtual-slot,
      duplicate-id-in-topics,
      duplicate-section-in-topic,
      empty-header-in-interchange-file,
      error-in-command-arguments,
      error-in-command-option,
      file-error,
      file-not-found,
      file-type-not-known,
      fully-qualified-name-not-found-in-code,
      id-matches-topic-title,
      illegal-character-in-id,
      illegal-section-in-topic,
      inconsistent-cpl,
      leading-colon-in-id,
      leading-colon-in-title,
      link-without-qv-or-vi-in-spec,
      multiple-libraries-in-fileset,
      multiple-topics-for-api,
      no-context-topic-in-block,
      no-definition-for-bindings,
      no-files-in-command-arguments,
      no-header-in-interchange-file,
      no-library-in-fileset,
      parse-error-in-dylan,
      parse-error-in-markup,
      q-and-qq-in-spec,
      qv-or-vi-in-title,
      sections-in-nonsection-markup,
      skipped-level-in-toc-file,
      target-not-found-in-link,
      topics-in-nontopic-markup,
      undefined-module-for-interchange-file,
      unparsable-expression-in-code,
      unsupported-syntax-in-code,
      unused-docs-in-topic,
      ;
end module;

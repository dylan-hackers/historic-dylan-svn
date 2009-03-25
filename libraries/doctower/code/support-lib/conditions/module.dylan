module: dylan-user

define module conditions
   use common-imports;
   
   export
      <skip-error-restart>, error-condition,
      <user-visible-warning>, <user-visible-error>, error-code, error-location,
      <need-locations>, specifier-for-locations;
   
   export
      ambiguous-title-in-link,
      bad-syntax-in-toc-file,
      circular-definition,
      conflicting-bindings-in-module,
      conflicting-definitions-in-code,
      conflicting-locations-in-tree,
      conflicting-modules-in-library,
      duplicate-id-in-topics,
      duplicate-modules-in-fileset,
      duplicate-section-in-topic,
      empty-header-in-interchange-file,
      error-in-command-option,
      file-error,
      file-not-found,
      file-type-not-known,
      id-matches-topic-title,
      illegal-character-in-id,
      illegal-section-in-topic,
      leading-colon-in-id,
      leading-colon-in-title,
      library-exports-not-known,
      multiple-libraries-in-fileset,
      no-context-topic-in-block,
      no-definition-for-bindings,
      no-definition-for-modules,
      no-files-in-command-arguments,
      no-header-in-interchange-file,
      no-library-in-fileset,
      parse-error-in-dylan,
      parse-error-in-markup,
      q-and-qq-in-spec,
      qv-or-vi-in-title,
      skipped-level-in-toc-file,
      target-not-found-in-link,
      undefined-module-for-interchange-file,
      unparsable-expression-in-code,
      unsupported-syntax-in-code,
      ;
end module;

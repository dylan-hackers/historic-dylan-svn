module: dylan-user

define module conditions
   use common-imports;
   
   export
      <user-visible-warning>, <user-visible-error>, error-code, error-location,
      <syntax-warning>, <syntax-error>, <design-error>,
      <need-locations>, specifier-for-locations;
   
   export
      ambiguous-title-in-link,
      bad-syntax-in-toc-file,
      conflicting-locations-in-tree,
      duplicate-id-in-topics,
      duplicate-section-in-topic,
      file-error,
      file-not-found,
      file-type-not-known,
      id-matches-topic-title,
      illegal-character-in-id,
      illegal-section-in-topic,
      leading-colon-in-id,
      leading-colon-in-title,
      multiple-libraries-in-fileset,
      no-context-topic-in-block,
      no-files-in-lid-file,
      no-library-in-fileset,
      no-modules-in-fileset,
      parse-error-in-dylan,
      parse-error-in-markup,
      q-and-qq-in-spec,
      qv-or-vi-in-title,
      skipped-level-in-toc-file,
      target-not-found-in-link,
      unparsable-expression-in-code,
      unsupported-syntax-in-code,
      ;
end module;

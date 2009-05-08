module: dylan-user

define module dylan-parser
   use common, exclude: { source-location };
   use parser-common, export: { source-location };
   use markup-parser, import: { <markup-content-token>, parse-markup };
   use conditions;

   // from peg-parser
   use peg-parser, export: { <token>, *parser-trace* };
   // from string-extensions
   use character-type;
   use substring-search;

   export parse-dylan;

   export
      <body-style-definition-rule-token>,
      <class-definer-token>,
      <constant-definer-token>,
      <create-clause-token>,
      <definition-token>,
      <domain-definer-token>,
      <export-clause-token>,
      <function-definer-token>,
      <function-rule-token>,
      <generic-definer-token>,
      <header-token>,
      <interchange-file-token>,
      <library-definer-token>,
      <list-style-definition-rule-token>,
      <macro-definer-token>,
      <method-definer-token>,
      <module-definer-token>,
      <property-token>,
      <renaming-token>,
      <source-record-token>,
      <statement-rule-token>,
      <text-name-token>,
      <text-token>,
      <use-clause-token>,
      <variable-definer-token>;
      
   export
      <accepts-keys-argument>,
      <all-keys-argument>,
      <class-keyword>,
      <class-slot>,
      <func-argument>,
      <func-param>,
      <func-value>,
      <keyword-argument>,
      <required-argument>,
      <required-singleton-argument>,
      <required-typed-argument>,
      <required-value>,
      <rest-argument>,
      <rest-value>;

   export
      api-modifiers,
      api-name,
      api-type,
      api-value,
      class-keywords,
      class-slots,
      class-supers,
      create-names,
      definitions,
      domain-types,
      export-names,
      func-options,
      func-params,
      func-values,
      hdr-keyword,
      hdr-value,
      headers,
      import-name,
      keyword-doc,
      keyword-init,
      keyword-name,
      keyword-required?,
      keyword-slot-name,
      keyword-type,
      local-name,
      main-rule-set,
      namespace-clauses,
      param-default,
      param-doc,
      param-instance,
      param-name,
      param-type,
      prop-name,
      prop-value,
      scoped-docs,
      slot-doc,
      slot-init,
      slot-modifiers,
      slot-name,
      slot-setter,
      slot-type,
      source-record,
      source-text,
      unscoped-docs,
      use-exclusions,
      use-exports,
      use-imports,
      use-name,
      use-prefix,
      use-renamings;
end module;

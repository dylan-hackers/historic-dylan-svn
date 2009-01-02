module: dylan-user

define module dylan-parser
   use common, exclude: { source-location };
   use parser-common, export: { source-location };
   use conditions;

   // from peg-parser
   use peg-parser, export: { <token>, *parser-trace* };
   // from string-extensions
   use character-type;
   // from wrapper-streams
   use canonical-text-stream;

   export parse-dylan-file;

   export
      <class-definer-token>,
      <constant-definer-token>,
      <definition-token>,
      <doc-comment-token>,
      <function-definer-token>,
      <generic-definer-token>,
      <header-token>,
      <method-definer-token>,
      <property-token>,
      <interchange-file-token>,
      <source-record-token>,
      <text-token>,
      <variable-definer-token>;
      
   export
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
      definitions,
      func-options,
      func-params,
      func-values,
      hdr-keyword,
      hdr-value,
      headers,
      keyword-doc,
      keyword-init,
      keyword-name,
      keyword-required?,
      keyword-type,
      param-default,
      param-doc,
      param-instance,
      param-name,
      param-type,
      scoped-docs,
      slot-doc,
      slot-init,
      slot-modifiers,
      slot-name,
      slot-setter,
      slot-type,
      source-record,
      unscoped-docs;
end module;

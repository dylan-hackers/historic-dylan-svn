module: dylan-user

define module dylan-rep
   use common;

   // from peg-parser
   use peg-parser, import: { <token> };
   
   export
      <documentable-api-element>, <named-api-element>, <source-name>,
      <library>, <known-library>, <unknown-library>,
      <module>, <local-module>, <imported-module>,
      <binding>, <local-binding>, <imported-binding>,
      <definition>, <class-defn>, <generic-defn>, <function-defn>,
      <constant-defn>, <variable-defn>, <macro-defn>,
      <explicit-class-defn>, <explicit-generic-defn>, <implicit-generic-defn>,
      <explicit-function-defn>, <explicit-constant-defn>, <explicit-variable-defn>,
      <explicit-macro-defn>, <explicit-body-macro-defn>, <explicit-list-macro-defn>,
      <explicit-stmt-macro-defn>, <explicit-func-macro-defn>,
      <fragment>, <computed-constant>, <type-fragment>, <code-fragment>, <name-fragment>
      ;

   export
      adjs,
      all-defns,
      bindings,
      bindings-setter,
      comment-tokens,
      definition,
      definition-setter,
      direct-supers,
      explicit-defn,
      explicit-defn-setter,
      exported?,
      exported?-setter,
      fragment-names,
      implicit-defns,
      implicit-defns-setter,
      import-name,
      import-name-setter,
      init-args,
      local-name,
      local-name-setter,
      modules,
      modules-setter,
      slots,
      source-name,
      source-text,
      stray?,
      unknown-reexport-sources,
      unknown-reexport-sources-setter,
      used-library,
      used-library-setter,
      used-module,
      used-module-setter,
      ;
end module;

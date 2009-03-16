module: dylan-user

define module dylan-rep
   use common;

   // from peg-parser
   use peg-parser, import: { <token> };
   
   export
      <library>, <known-library>, <unknown-library>,
      <module>, <local-module>, <imported-module>,
      <binding>, <local-binding>, <imported-binding>,
      <definition>, <class-defn>, <generic-defn>, <function-defn>,
      <constant-defn>, <variable-defn>, <macro-defn>,
      <explicit-class-defn>, <explicit-generic-defn>, <implicit-generic-defn>,
      <explicit-function-defn>, <explicit-constant-defn>, <explicit-variable-defn>,
      <explicit-macro-defn>, <explicit-body-macro-defn>, <explicit-list-macro-defn>,
      <explicit-stmt-macro-defn>, <explicit-func-macro-defn>
      ;

   export
      local-name, local-name-setter, import-name, import-name-setter, modules,
      modules-setter, definitions, definitions-setter, used-library,
      used-library-setter, bindings, bindings-setter, used-module,
      used-module-setter, definition, definition-setter,
      unknown-reexport-sources, unknown-reexport-sources-setter, exported?,
      exported?-setter, stray?, implicit-defns, implicit-defns-setter,
      explicit-defn, explicit-defn-setter
      ;
end module;

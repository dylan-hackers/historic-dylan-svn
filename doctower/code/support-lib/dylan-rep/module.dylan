module: dylan-user

define module dylan-rep
   use common;

   // from peg-parser
   use peg-parser, import: { <token> };
   
   export
      <library>, <known-library>, <unknown-library>,
      <module>, <local-module>, <imported-module>,
      <binding>, <local-binding>, <imported-binding>,
      <definition>
      ;

   export
      local-name, local-name-setter, import-name, import-name-setter,
      used-libraries, used-libraries-setter, modules, modules-setter,
      definitions, definitions-setter, used-library, used-library-setter,
      bindings, bindings-setter, used-module, used-module-setter, definition,
      definition-setter, unknown-reexport-sources, unknown-reexport-sources-setter,
      exported?, exported?-setter, anonymous?
      ;
end module;

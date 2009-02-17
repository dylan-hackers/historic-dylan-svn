module: dylan-user

define module dylan-rep
   use common;

   // from peg-parser
   use peg-parser, import: { <token> };
   
   export
      <library>, <unknown-library>, <module>, <exported-module>,
      <reexported-module>, <imported-module>, <internal-module>,
      <binding>, <exported-binding>, <reexported-binding>, <imported-binding>
      ;

   export
      local-name, local-name-setter, import-name, import-name-setter,
      used-libraries, used-libraries-setter, modules, modules-setter,
      definitions, definitions-setter, used-library, used-library-setter,
      bindings, bindings-setter, used-module, used-module-setter, definition,
      definition-setter
      ;
end module;

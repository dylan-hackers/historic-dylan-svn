module: dylan-user

define module dylan-rep
   use common;

   // from peg-parser
   use peg-parser, import: { <token> };
   
   export
      <explicit-api>, <library>, <unknown-library>, <module>, <exported-module>,
      <reexported-module>, <imported-module>, <internal-module>
      ;

   export
      local-name, local-name-setter, import-name, import-name-setter,
      used-libraries, used-libraries-setter, modules, modules-setter,
      definitions, definitions-setter, used-library, used-library-setter,
      bindings, bindings-setter, source-token
      ;
end module;

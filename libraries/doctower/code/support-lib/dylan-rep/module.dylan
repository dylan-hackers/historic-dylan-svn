module: dylan-user

define module dylan-rep
   use common;

   // from peg-parser
   use peg-parser, import: { <token> };
   
   export
      <explicit-api>, <library>, <used-library>, <exported-module>,
      <reexported-module>, <imported-module>, <internal-module>
      ;

   export
      local-name, local-name-setter, import-name, import-name-setter,
      used-libraries, source-token
      ;
end module;

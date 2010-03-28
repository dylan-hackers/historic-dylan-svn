module: dylan-user
synopsis: This module includes a few functions for processing names and IDs.

define module name-processing
   use common, rename: { binding-name => dyn-binding-name };
   use dylan-rep;
   use markup-rep;
   // from regular-expressions
   use regular-expressions, import: { regexp-replace };
   // from string-extensions
   use character-type;
   
   export
      standardize-qualified-name, standardize-id, standardize-title,
      as-titlecase;
end module;

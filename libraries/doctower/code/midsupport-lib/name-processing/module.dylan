module: dylan-user
synopsis: This module includes a few functions for processing names and IDs.

define module name-processing
   use common, rename: { binding-name => dyn-binding-name };
   // from regular-expressions
   use regular-expressions, import: { regexp-replace };
   // from string-extensions
   use character-type;
   // from collection-extensions
   use vector-search, import: { find-last-key };
   
   export
      standardize-qualified-name, standardize-id, standardize-title,
      as-titlecase, enclosing-qualified-name, id-matches-qualified-name?;
end module;

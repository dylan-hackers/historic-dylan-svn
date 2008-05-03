module: dylan-user

/// Synopsis: Controls other modules to read files and generate the internal
/// model of the documentation.
define module workflows
   use common;
   use ordered-tree;
   use markup-parser, import: { parse-markup };
   use markup-translator;
   use topic-resolver;
   
   export
      create-doc-tree;
end module;

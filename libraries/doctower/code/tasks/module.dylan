module: dylan-user

/// Synopsis: Controls other modules to read files and generate the internal
/// model of the documentation.
define module workflows
   use common;
   use conditions;
   use ordered-tree;
   use source-files;
   use topic-resolver;
   
   export
      create-doc-tree;
end module;

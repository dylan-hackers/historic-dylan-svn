module: dylan-user

/// Synopsis: Controls other modules to read files and generate the internal
/// model of the documentation.
define module tasks
   use common;
   use conditions;
   use template-files;
   use source-files;
   use topic-resolver;
   use ordered-tree;
   
   export
      create-doc-tree;
end module;

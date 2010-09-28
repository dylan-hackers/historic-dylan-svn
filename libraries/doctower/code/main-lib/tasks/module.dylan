module: dylan-user

/// Synopsis: Controls other modules to read files and generate the internal
/// model of the documentation and output.
define module tasks
   use common;
   use conditions;
   use template-files;
   use source-files;
   use topic-resolver;
   use output;
   use ordered-tree;
   
   export
      create-doc-tree, create-output-files;
end module;

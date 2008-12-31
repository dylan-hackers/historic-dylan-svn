module: dylan-user

define module ordered-tree
   use common, import: { log, log-object };
   // from common-dylan
   use dylan;
   use common-extensions;
   // from io
   use streams, import: { <stream>, write, write-element };
   use format, import: { format };
   use print, import: { print-object };
   use pprint, import: { printing-logical-block, pprint-newline, pprint-indent };
   // from collection-extensions
   use vector-search;
   
   export
      <ordered-tree>, <ordered-tree-key>,
      key-depth, root-key, sup-key, inf-key-sequence, next-inf-key,
      succ-key, pred-key,
      copy-tree, replace-subtree!, sort-tree!;
end module;

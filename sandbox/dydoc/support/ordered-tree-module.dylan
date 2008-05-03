module: dylan-user

define module ordered-tree
   // from common-dylan
   use dylan;
   use common-extensions;
   // from io
   use streams, import: { <stream> };
   use format, import: { format };
   use print, import: { print-object };
   // from collection-extensions
   use vector-search;
   
   export
      <ordered-tree>, <ordered-tree-key>,
      root-key, sup-key, inf-key-sequence, next-inf-key,
      succ-key, pred-key,
      copy-tree, replace-subtree!
end module;

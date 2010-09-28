module: dylan-user
synopsis: Merges generated and authored topics and resolves links.

define module topic-resolver
   use common;
   use conditions;
   use markup-rep;
   use ordered-tree;
   use name-processing;
   use regular-expressions, import: { regexp-replace };
   use transcendental, import: { log => math-log };
   
   export
      group-mergeable-topics, check-and-merge-topics,
      resolution-info, resolve-target-placeholders, replace-content-placeholders,
      arrange-topics, ensure-topic-ids;
end module;

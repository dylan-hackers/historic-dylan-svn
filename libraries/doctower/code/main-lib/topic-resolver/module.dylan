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
      check-topic-ids, ensure-topic-ids, topics-by-id, topics-by-fqn, topics-by-title,
      resolve-xref-placeholders, resolve-topic-placeholders, arrange-topics,
      group-mergeable-topics, check-and-merge-topics,
      replace-content-placeholders;
end module;

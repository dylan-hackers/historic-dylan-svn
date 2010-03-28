module: dylan-user
synopsis: Merges generated and authored topics and resolves links.

define module topic-resolver
   use common;
   use conditions;
   use markup-rep;
   use ordered-tree;
   
   export
      resolve-topic-placeholders, arrange-topics, group-mergeable-topics,
      check-and-merge-topics;
end module;

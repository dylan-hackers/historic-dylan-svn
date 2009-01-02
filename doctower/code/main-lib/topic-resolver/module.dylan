module: dylan-user
synopsis: Merges implicit and explicit topics and resolves links.

define module topic-resolver
   use common;
   use conditions;
   use internal-rep;
   use ordered-tree;
   
   export
      resolve-topic-placeholders, arrange-topics, merge-topics;
end module;
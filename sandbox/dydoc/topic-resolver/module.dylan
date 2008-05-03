module: dylan-user
synopsis: Merges implicit and explicit topics and resolves links.

define module topic-resolver
   use common;
   use internal-rep;
   use ordered-tree;

   // from grammar
   use digraph, import: { find-rooted-scc };
   
   export
      resolve-topic-placeholders, arrange-topics, merge-topics;
end module;
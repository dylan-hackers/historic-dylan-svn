module: dylan-user
synopsis: This module creates topics from Dylan code representation.

define module dylan-topics
   use common, rename: { binding-name => dyn-binding-name };
   use dylan-rep;
   use markup-rep;

   export
      topics-from-dylan;
end module;
module: dylan-user
synopsis: This module creates topics from Dylan code representation.

define module dylan-topics
   use common;
   use dylan-rep;
   use markup-rep;

   export
      topics-from-dylan;
end module;
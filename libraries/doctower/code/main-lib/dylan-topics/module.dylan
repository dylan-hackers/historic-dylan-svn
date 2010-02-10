module: dylan-user
synopsis: This module creates topics from Dylan code representation.

define module dylan-topics
   use common, rename: { binding-name => dyn-binding-name };
   use regular-expressions, import: { regexp-replace };
   use character-type;
   use dylan-rep;
   use markup-rep;
   use dylan-translator, import: { $object-type };
   use markup-translator;

   export
      topics-from-dylan, $api-list-filename;
end module;
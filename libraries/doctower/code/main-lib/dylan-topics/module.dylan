module: dylan-user
synopsis: This module creates topics from Dylan code representation.

define module dylan-topics
   use common, rename: { binding-name => dyn-binding-name };
   use dylan-rep;
   use markup-rep;
   use markup-parser, import: { parse-internal-markup };
   use dylan-translator, import: { $object-type };
   use markup-translator;
   use template-files;
   use name-processing;
   // from wrapper-streams
   use canonical-text-stream;
   // from template-engine
   use template-engine;

   export
      topics-from-dylan, $api-list-filename;
end module;
module: dylan-user
synopsis: This module creates topics from Dylan code representation.

define module dylan-topics
   use common, rename: { binding-name => dyn-binding-name };
   use configs,
      import: { *api-list-file*, *generated-topics-directory*, *topic-file-extension* };
   use conditions;
   use dylan-rep;
   use markup-rep;
   use markup-parser,
      import: { parse-internal-markup, topics => token-topics,
                source-location => token-src-loc };
   use dylan-translator, import: { $object-type };
   use markup-translator;
   use template-files;
   use name-processing;
   // from template-engine
   use template-engine;
   // from system
   use file-system, import: { ensure-directories-exist };

   export
      topics-from-dylan, $topic-templates;
end module;

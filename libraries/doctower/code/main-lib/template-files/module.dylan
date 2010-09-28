module: dylan-user

define module template-files
   use common;
   use configs, import: { *topic-template-directory* };
   // from system
   use file-system, import: { file-exists?, file-property };
   // from template-engine
   use template-engine;
   // from wrapper-streams
   use canonical-text-stream;
   // from peg-parser
   use peg-parser,
      import: { <parse-failure>, parse-expected, parse-position, *parser-trace* };
   
   export
      topic-template, create-topic-templates, discard-topic-templates;
end module;

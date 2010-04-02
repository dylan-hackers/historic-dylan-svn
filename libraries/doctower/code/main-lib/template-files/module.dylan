module: dylan-user

define module template-files
   use common;
   // from template-engine
   use template-engine;
   // from peg-parser
   use peg-parser,
      import: { <parse-failure>, parse-expected, parse-position, *parser-trace* };
   
   export
      topic-template, create-topic-templates, discard-topic-templates,
      $topic-template-path;
end module;

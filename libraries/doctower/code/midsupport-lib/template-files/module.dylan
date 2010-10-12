module: dylan-user

define module template-files
   use common;
   use configs, import: { *template-directory* };
   use parser-common, import: { canonical-text-stream };
   // from system
   use file-system, import: { file-exists?, file-property };
   // from template-engine
   use template-engine;
   // from peg-parser
   use peg-parser,
      import: { <parse-failure>, parse-expected, parse-position, *parser-trace* };
   
   export
      template-by-name, create-templates, discard-templates;
end module;

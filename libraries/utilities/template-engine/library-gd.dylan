module: dylan-user

define library template-engine
   use common-dylan;
   use collections;
   use io;
   use peg-parser;
   use string-extensions;
   use sequence-stream;

   export template-engine;
end library;


define module template-engine
   // from common-dylan
   use dylan;
   use common-extensions, exclude: { format-to-string };
   // from collections
   use table-extensions,
      rename: { case-insensitive-equal => case-insensitive-equal? };
   // from io
   use streams, exclude: { <string-stream> };
   use format;
   use format-out;
   // from sequence-stream
   use sequence-stream, import: { <string-stream> };
   // from peg-parser
   use peg-parser;
   // from string-extensions
   use character-type;
   
   export
      <template>, process-template, template-vocabulary, vocabulary-table-type;
   
   export
      first-rep?, last-rep?, rep-number, rep-key;
   
   export
      <missing-name-error>, <expression-error>, error-position, actual-error,
      missing-name, missing-operation?;
end module;

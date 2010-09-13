module: dylan-user

define library template-test
   use common-dylan;
   use system;
   use io;
   use peg-parser;
   use table-extensions;

   use template-engine;
end library;


define module template-test
   use common-dylan;
   use file-system;
   use standard-io;
   use format-out;
   use table-extensions;
   use streams;
   use peg-parser, import: { *parser-trace*, *parser-cache-hits* };
   
   use template-engine;
end module;

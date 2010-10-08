module: dylan-user

define library midsupport-library
   use support-library;
   use peg-parser;
   use regular-expressions;
   use string-extensions;
   use collection-extensions;
   use system;
   use template-engine;
   use wrapper-streams;

   export configs, name-processing, parser-common, template-files;
end library;

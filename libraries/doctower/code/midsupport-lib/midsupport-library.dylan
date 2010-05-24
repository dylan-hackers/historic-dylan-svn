module: dylan-user

define library midsupport-library
   use support-library;
   use peg-parser;
   use regular-expressions;
   use string-extensions;
   use collection-extensions;

   export configs, name-processing, parser-common;
end library;

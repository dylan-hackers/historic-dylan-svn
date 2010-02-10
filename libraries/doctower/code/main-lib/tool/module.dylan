module: dylan-user

define module main
   use common;
   use conditions;
   use tasks;
   use markup-parser, import: { *parser-trace* };
   use dylan-topics, import: { $api-list-filename };
   
   // from io
   use format-out;
   use print;
   use pprint, import: { *default-line-length* };
   // from command-line-parser
   use command-line-parser;
   // from dylan
   use extensions, import: { report-condition };
   // from system
   use file-system, import: { <file-does-not-exist-error> };
   use locators, import: { locator-extension };
end module;

module: dylan-user

define library dylan-parser-library
   use support-library;
   use midsupport-library, import: { parser-common };
   use markup-parser-library;
   use peg-parser;
   use string-extensions;
   export dylan-parser;
end library;
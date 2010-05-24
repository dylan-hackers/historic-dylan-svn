module: dylan-user

define library markup-rep-library
   use support-library;
   use midsupport-library;
   use markup-parser-library;
   use system;
   
   export markup-rep, markup-translator;
end library;

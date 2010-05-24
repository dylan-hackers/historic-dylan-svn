module: dylan-user

define library dylan-rep-library
   use support-library;
   use dylan-parser-library;
   use markup-parser-library;
   use markup-rep-library, import: { markup-rep };
   
   export dylan-rep, dylan-translator;
end library;

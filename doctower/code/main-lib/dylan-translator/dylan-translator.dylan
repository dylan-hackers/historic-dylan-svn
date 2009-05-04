module: dylan-translator


/** Synopsis: Returns topics for Dylan libraries, modules, definitions, etc. **/
define method topics-from-dylan (library-sets :: <sequence>)
=> (topics :: <sequence>)
   // Model source code.
   let libraries = apis-from-dylan(library-sets);
   
   // Generate implicit topics and sections.
   // Generate explicit topics, sections, and placeholders.
   #[]
end method;


module: dylan-translator


/** Synopsis: Returns topics for Dylan libraries, modules, definitions, etc. **/
define method topics-from-dylan (library-sets :: <sequence>)
=> (topics :: <sequence>)
   // TODO
   // Model source code.
   let libraries = apis-from-dylan(library-sets);
   
   // Generate implicit topics and sections.
   // Generate explicit topics, sections, and placeholders.
   #[]
end method;


/**
Synopsis: Returns representation of Dylan libraries, modules, definitions, etc.

A library definition lists used libraries and modules. Modules may be created
and exported from the current library, created but internal to the current
library, excluded from import from another library, imported and re-exported
from another library, imported and re-exported under a different name from
another library, or imported but not re-exported from another library. There can
be multiple use clauses for the same library.

   Exported    - Module is listed in export clause. Instance of <exported-module>.
   Internal    - Module is created via "define module" but not mentioned in
                 "define library". Instance of <internal-module>.
   Excluded    - Module is listed in use clause exclude option, or not listed in
                 use clause import option. No representation.
   Reexported  - Module is listed in use clause export option. Instance of
                 <reexported-module>.
   Renamed     - Module is listed in use clause export option and given a new
                 name with rename option, import option, or prefix option.
                 Instance of <reexported-module>.
   Imported    - Module is listed in use clause import option, but not listed
                 in export option. Renaming these is possible but has no effect.
                 Instance of <imported-module>.

The intent of processing a library definition is to create a list of exported
modules for the given library, with the proper bindings in each module. The only
real fact of interest in the documentation of the library itself is the modules
being exported, so we can ignore excluded and imported modules and the original
names of renamed modules.

However, this information needs to be preserved for the creation of module
documentation; a module will need to refer to its used modules for reexported
bindings.

Arguments:
   library-sets - A sequence. Each element of the sequence is a library being
                  documented. Specifically, each element is a sequence of the
                  parsed files included in that library; a sequence of
                  <interchange-file-token>.

Values:
   libraries - A sequence of <library> objects, each of which contains all the
               modules, bindings, and definitions of that library.
**/
define method apis-from-dylan (library-sets :: <sequence>)
=> (representations :: <sequence>)

   // Find libraries and sort them by dependency.
   let libraries = map(extract-library, library-sets);
   let libraries = dependent-libraries(libraries);
   log-object("Library dependencies", libraries);
   
   // Create modules in the libraries.

   // Sort APIs into module and library.

   #[]
end method;


define constant <non-namespace-definition-token> =
      type-union(<class-definer-token>, <constant-definer-token>,
                 <function-definer-token>, <generic-definer-token>,
                 <method-definer-token>, <variable-definer-token>);


define method choose-interchange-definitions
   (type :: <type>, ichange-tokens :: <sequence> /* of <interchange-file-token> */)
=> (seq :: <sequence> /* of type */)
   let source-records = choose(true?, map(source-record, ichange-tokens));
   let source-defns = map(definitions, source-records);
   let defns = apply(concatenate, #[], source-defns);
   choose(rcurry(instance?, type), defns);
end method;

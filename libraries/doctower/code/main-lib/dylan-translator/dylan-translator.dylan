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

--- Arguments: ---
file-sets - A sequence. Each element of the sequence is a library being
            documented. Specifically, each element is a sequence of the parsed
            files included in that library; a sequence of
            <interchange-file-token>.

--- Values: ---
libraries - A sequence of <library> objects, each of which contains all the
            modules, bindings, and definitions of that library.
**/
define method apis-from-dylan (file-sets :: <sequence>)
=> (libraries :: <sequence>)
   let library-sets :: <table> = make-library-sets(file-sets);
   verbose-log("Cataloging definitions");

   // First pass. Populate each library with modules.
   do(method (lib-token :: <library-definer-token>)
         find/make-library(library-sets, lib-token, lib-token.api-name)
      end, map(libset-library-token, library-sets));

   // Second pass. Make sure all imports and exports are included.
   do(compose(curry(check-library-imports-exports, library-sets), libset-library),
      choose(libset-library-token, as(<vector>, library-sets)));
   
   /**/
   log-object("Libraries", map(libset-library, as(<vector>, library-sets)));
   for (lib in map(libset-library, as(<vector>, library-sets)))
      log-object(format-to-string("Modules in %s", lib.local-name), lib.modules)
   end for;
   
   // Do bindings. This has to happen after modules are figured, because some
   // modules are merged together.
   // TODO
   
   map(libset-library, as(<vector>, library-sets));
end method;

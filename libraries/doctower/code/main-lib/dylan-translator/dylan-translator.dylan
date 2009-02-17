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
library-sets - A sequence. Each element of the sequence is a library being
               documented. Specifically, each element is a sequence of the
               parsed files included in that library; a sequence of
               <interchange-file-token>.

--- Values: ---
libraries - A sequence of <library> objects, each of which contains all the
            modules, bindings, and definitions of that library.
**/
define method apis-from-dylan (library-sets :: <sequence>)
=> (representations :: <sequence>)
   // Find libraries and sort them by dependency.
   let library-sets = map(extract-library, library-sets);
   let library-sets = dependent-libraries(library-sets);
   
   // Create modules in the libraries.
   populate-modules(library-sets);
   
   // Sort APIs into module and library.
   populate-bindings(library-sets);

   map(head, library-sets);
end method;


define constant <non-namespace-definition-token> =
      type-union(<class-definer-token>, <constant-definer-token>,
                 <function-definer-token>, <generic-definer-token>,
                 <method-definer-token>, <variable-definer-token>);


define method choose-interchange-definitions
   (type :: <type>, ichange-tokens :: <sequence> /* of <interchange-file-token> */)
=> (seq :: <sequence> /* of type */)
   let source-records = choose(true?, map(source-record, ichange-tokens));
   let source-defns = map(token-definitions, source-records);
   let defns = apply(concatenate, #[], source-defns);
   choose(rcurry(instance?, type), defns);
end method;

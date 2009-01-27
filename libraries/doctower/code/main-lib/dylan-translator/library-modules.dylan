module: dylan-translator


define method extract-library (files :: <sequence>)
=> (library :: <library>)
   let libs = choose-interchange-definitions(<library-definer-token>, files);
   select (libs.size)
      0 =>
         let files = map(source-file, map(token-src-loc, files));
         no-library-in-fileset(#f, filenames: item-string-list(files));
      1 =>
         let token = libs.first;
         make(<library>, source-token: token, local-name: token.api-name);
      otherwise =>
         let dupes = map(token-src-loc, libs);
         multiple-libraries-in-fileset(#f, defn-locations: item-string-list(dupes));
   end select;
end method;


/**
Synopsis: Returns list of libraries, those with no dependencies first.

The algorithm here is as follows:
   1. Give each library a row of a table. Contents of the row is the list of
      libraries used by that library.
   2. Go through each used library list. If a library in the list has a row,
      recursively insert its dependencies' dependencies, then its dependencies,
      then the library.
   3. When finished, each library will have a complete dependency list. Return
      the libraries in ascending order of number of dependencies; since each
      dependency list is complete and each library in each list in preceded by
      its dependencies, there should be no issues.
**/
define method dependent-libraries (libraries :: <sequence> /* of <library> */)
=> (ordered-libraries :: <sequence>)
   let dependencies = make(<case-insensitive-string-table>);
   for (lib in libraries)
      let token :: <library-definer-token> = lib.source-token;
      let clauses = token.namespace-clauses;
      let use-clauses = choose(rcurry(instance?, <use-clause-token>), clauses);
      dependencies[lib.local-name] :=
            remove-duplicates!(map(use-name, use-clauses), test: case-insensitive-equal?);
   end for;
   
   local method library-dependencies
            (dependent-list :: <stretchy-vector>, lib :: <string>)
         => (dependent-list :: <stretchy-vector>)
            let used-libs = element(dependencies, lib, default: #[]);
            for (used-lib in used-libs)
               dependent-list := library-dependencies(dependent-list, used-lib);
            end for;
            add-new!(dependent-list, lib, test: case-insensitive-equal?);
         end method;
   
   let full-dependencies = make(<case-insensitive-string-table>);
   for (lib-name in dependencies.key-sequence)
      let ordered-dependents = library-dependencies(make(<stretchy-vector>), lib-name);
      full-dependencies[lib-name] := ordered-dependents;
   end for;
   
   local method compare-dependency-size (lib1 :: <library>, lib2 :: <library>)
         => (lib1-less? :: <boolean>)
            let lib1-deps = full-dependencies[lib1.local-name].size;
            let lib2-deps = full-dependencies[lib2.local-name].size;
            lib1-deps < lib2-deps;
         end method;

   sort(libraries, test: compare-dependency-size);
end method;


// define method extract-modules (files :: <sequence>, library :: <library>)
// => (modules :: <sequence> /* of <module> */)
//    let mod-tokens = choose-interchange-definitions(<module-definer-token>, files);
//    select (mod-tokens.size)
//       0 =>
//          let files = map(source-file, map(token-src-loc, files));
//          no-modules-in-fileset(#f, filenames: item-string-list(files));
//       otherwise =>
//          local method make-module (token) => (module)
//                   make(<module>, source-token: token, local-name: token.api-name)
//                end method;
//          map(make-module, mod-tokens);
//    end select;
// end method;
// 
// 
// define method process-library
//    (token :: <library-definer-token>, files :: <sequence>)
// => (library :: <library>)
//    let clauses = token.namespace-clauses;
//    let definitions =
//          choose-interchange-definitions(<non-namespace-definition-token>, files);
// 
//    // Catalog modules from use clauses.
//    
//    let use-clauses = choose(rcurry(instance?, <use-clause-token>), clauses);
//    let used-lib-names = map(use-name, use-clauses);
//    map-into(library.used-libraries, curry(make, <used-library>, import-name:),
//             used-lib-names);
//             
//    
// 
//    let export-clauses = choose(rcurry(instance?, <export-clause-token>), clauses);
//    let export-module-names = map(export-names, export-clauses);
//    let export-module-names = apply(concatenate, #[], export-module-names);
//    let export-modules = map(curry(apply, <module>, local-name:), export-module-names);
// 
//    let modules = extract-modules(files, library);
// 
//    library
// end method;
// 
// 
// define method process-module
//    (token :: <module-definer-token>, library :: library>)
// => (module :: <module>)
// end method;

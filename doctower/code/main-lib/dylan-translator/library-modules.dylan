module: dylan-translator


define method extract-library (files :: <sequence>)
=> (library-set :: <pair> /* of <library>, <sequence> of <interchange-file-token> */)
   let libs = choose-interchange-definitions(<library-definer-token>, files);
   let library =
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
   pair(library, files)
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

--- Arguments: ---
library-sets - A sequence of <pair>. The head of each pair is a <library> and 
               the tail is a sequence of <interchange-file-token>.

--- Values: ---
ordered-library-sets - As above, but in order of dependency. Libraries that are
                       dependent on other libraries under consideration come last.
**/
define method dependent-libraries (library-sets :: <sequence>)
=> (ordered-library-sets :: <sequence>)
   let libraries = map(head, library-sets);

   // Step 1
   
   let dependencies = make(<case-insensitive-string-table>);
   for (lib in libraries)
      // Get used libraries from use clauses.
      let token :: <library-definer-token> = lib.source-token;
      let clauses = token.namespace-clauses;
      let use-clauses = choose(rcurry(instance?, <use-clause-token>), clauses);
      let dependency-list = 
            remove-duplicates!(map(use-name, use-clauses), test: case-insensitive-equal?);
      dependencies[lib.local-name] := dependency-list;

      // Add to <library> instance's used-library list.
      for (dependency in dependency-list)
         let used-lib =
               block (found)
                  for (lib in libraries)
                     if (case-insensitive-equal?(lib.local-name, dependency))
                        found(lib)
                     end if;
                  end for;
                  found(make(<unknown-library>, local-name: dependency));
               end block;
         lib.used-libraries := add!(lib.used-libraries, used-lib);
      end for;
   end for;
   
   // Step 2
   
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
   
   // Step 3
   
   local method compare-dependency-size (set1 :: <pair>, set2 :: <pair>)
         => (set1-less? :: <boolean>)
            let lib1-deps = full-dependencies[set1.head.local-name].size;
            let lib2-deps = full-dependencies[set2.head.local-name].size;
            lib1-deps < lib2-deps;
         end method;

   sort(library-sets, test: compare-dependency-size);
end method;


/**
Synopsis: Creates modules in libraries.

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

The function operates as follows:
   1. Populate the libraries with exported and internal modules derived from
      export clauses.
   2. Populate the libraries with imported, renamed, and reexported clauses
      listed in use clauses and carried forward from the other libraries.

--- Arguments: ---
library-sets - A sequence of <pair>. The head of each pair is a <library> and 
               the tail is a sequence of <interchange-file-token>. The libraries
               should be in order of dependency so that re-exported modules of
               earlier libraries are available for import by later libraries.
**/
define method populate-modules (library-sets :: <sequence>) => ()
   let libraries = map(head, library-sets);
   let libraries-by-name = make(<case-insensitive-string-table>, size: library-sets.size);
   let library-export-clauses = make(<vector>, size: library-sets.size);
   let library-use-clauses = make(<vector>, size: library-sets.size);
   let library-module-tokens = make(<vector>, size: library-sets.size);
   
   for (library-set keyed-by i in library-sets)
      let library = library-set.head;
      let token = library.source-token;
      let module-tokens = extract-module-tokens(library-set.tail);
      let clauses = token.namespace-clauses;
      let export-clauses = choose(rcurry(instance?, <export-clause-token>), clauses);
      let use-clauses = choose(rcurry(instance?, <use-clause-token>), clauses);
      libraries-by-name[library.local-name] := library;
      library-export-clauses[i] := export-clauses;
      library-use-clauses[i] := use-clauses;
      library-module-tokens[i] := module-tokens;
   end for;

   // Step 1
   
   for (library keyed-by i in libraries)
      library.modules := concatenate!(library.modules,
            modules-from-export-clauses(library-export-clauses[i],
                                        library-module-tokens[i]));
   end for;
   
   // Step 2
   
   for (library keyed-by i in libraries)
      for (clause in library-use-clauses[i])
         library.modules := concatenate!(library.modules,
               modules-from-use-clause(clause, libraries-by-name));
      end for;
   end for;
   
   for (lib in libraries)
      log("Library %s uses libraries %=", lib, lib.used-libraries);
      log("Library %s contains modules %=", lib, lib.modules);
   end for;

   // map(merge-modules, libraries);
end method;


define method extract-module-tokens (files :: <sequence>)
=> (tokens :: <sequence> /* of <module-definer-token> */)
   let mod-tokens = choose-interchange-definitions(<module-definer-token>, files);
   when (mod-tokens.size = 0)
      let files = map(source-file, map(token-src-loc, files));
      no-modules-in-fileset(#f, filenames: item-string-list(files));
   end when;
   mod-tokens
end method;


define method modules-from-export-clauses
   (export-clauses :: <sequence>, module-tokens :: <sequence>)
=> (modules :: <sequence>)
   let names-per-export-clause = map(export-names, export-clauses);
   let exports = remove-duplicates!(apply(concatenate, #[], names-per-export-clause),
                                    test: case-insensitive-equal?);

   local method exported-token? (token :: <module-definer-token>)
         => (exported? :: <boolean>)
            member?(token.api-name, exports, test: case-insensitive-equal?)
         end method,
         
         method make-module (token :: <module-definer-token>, exported? :: <boolean>)
         => (module :: <module>)
            make(if (exported?) <exported-module> else <internal-module> end,
                 local-name: token.api-name, source-token: token)
         end method;
         
   map(method (token :: <module-definer-token>) => (mod :: <module>)
          make-module(token, token.exported-token?)
       end, module-tokens);
end method;


define method modules-from-use-clause
   (use-clause :: <use-clause-token>, libraries :: <table> /* of <string> => <library> */)
=> (modules :: <sequence>)
   let import-options :: type-union(<sequence> /* of <renaming-token> */, singleton(#"all"))
         = use-clause.use-imports | #"all";
   let rename-options :: <sequence> /* of <renaming-token> */
         = use-clause.use-renamings | #[];
   let exclude-options :: <sequence> /* of <string> */
         = use-clause.use-exclusions | #[];
   let export-options :: type-union(<sequence> /* of <string> */, singleton(#"all"))
         = use-clause.use-exports | #[];
   let prefix-option :: false-or(<string>)
         = use-clause.use-prefix;
   
   let import-and-rename-options :: <sequence> /* of <renaming-token> */ =
         concatenate(if (import-options = #"all") #[] else import-options end,
                     rename-options);
   
   // Get names of modules from used library.
   
   let from-library = element(libraries, use-clause.use-name, default: #f);
   if (~from-library & export-options ~= #[])
      library-exports-not-known(use-clause.token-src-loc);
   end if;
   
   let all-modules = (from-library & from-library.modules) | #[];
   let all-modules = map(local-name, all-modules);

   // Remove excluded modules.
   
   let included-modules = difference(all-modules, exclude-options,
                                     test: case-insensitive-equal?);
   
   // If we don't have an actual module list for the library, assume it has
   // any modules explicitly named in rename or import options.
   
   when (~from-library)
      let imported-names = map(token-import-name, import-and-rename-options);
      included-modules := concatenate!(included-modules, imported-names);
   end when;
   
   // Only include imported modules.
   
   let imported-modules =
         if (import-options = #"all")
            included-modules
         else
            let imported-names = map(token-import-name, import-options);
            intersection(included-modules, imported-names,
                         test: case-insensitive-equal?)
         end if;
   
   // Utility functions.

   local method import-name-of-renaming?
            (name :: <string>, renaming :: <renaming-token>)
         => (match? :: <boolean>)
            case-insensitive-equal?(name, renaming.token-import-name)
         end method,
         
         method local-name-of-renaming?
            (name :: <string>, renaming :: <renaming-token>)
         => (match? :: <boolean>)
            case-insensitive-equal?(name,
                  renaming.token-local-name | renaming.token-import-name)
         end method,
         
         method local-name-of-renaming (renaming :: <renaming-token>)
         => (local-name :: <string>)
            if (renaming.token-local-name)
               renaming.token-local-name
            elseif (prefix-option)
               concatenate(prefix-option, renaming.token-import-name)
            else
               renaming.token-import-name
            end if;
         end method,
         
         method local-names-for-imported-module (import-name :: <string>)
         => (local-names :: <sequence> /* of <string> */)
            let renamings = choose(curry(import-name-of-renaming?, import-name),
                                   import-and-rename-options);
            case
               ~renamings.empty? => map(local-name-of-renaming, renamings);
               prefix-option => vector(concatenate(prefix-option, import-name));
               otherwise => vector(import-name);
            end case
         end method,
         
         method import-name-for-exported-module (export-name :: <string>)
         => (import-name :: <string>)
            let renamings = choose(curry(local-name-of-renaming?, export-name),
                                   import-and-rename-options);
            let renaming = element(renamings, 0, default: #f);
            case
               renaming => renaming.token-import-name;
               prefix-option =>
                  if (case-insensitive-equal?(prefix-option,
                        copy-sequence(export-name, end: prefix-option.size)))
                     copy-sequence(export-name, start: prefix-option.size)
                  else
                     export-name
                  end if;
               otherwise => export-name;
            end case
         end method;
               
   // Determine local name or names (if multiple import/rename options) of
   // each imported module.

   let module-import-names :: <sequence> /* of <string> */
         = remove-duplicates!(included-modules, test: case-insensitive-equal?);
   let module-local-names :: <sequence> /* of <sequence> of <string> */
         = map(local-names-for-imported-module, module-import-names);

   // Assume used library has any modules named in export option that aren't
   // already known.

   let all-module-local-names = apply(concatenate, #[], module-local-names);
   let exported-modules :: <sequence> /* of <string> */
         = if (export-options = #"all") all-module-local-names else export-options end;
   let unknown-exports :: <sequence> /* of <string> */
         = difference(exported-modules, all-module-local-names,
                      test: case-insensitive-equal?);

   for (export-name in unknown-exports)
      module-import-names := add!(module-import-names,
                                  import-name-for-exported-module(export-name));
      module-local-names := add!(module-local-names, vector(export-name));
   end for;

   // Make module list.

   let used-lib = from-library | make(<unknown-library>, local-name: use-clause.use-name);
   let module-list = make(<stretchy-vector>);
   for (import-name in module-import-names, local-names in module-local-names)
      for (local-name in local-names)
         let module-class = 
               if (member?(local-name, exported-modules, test: case-insensitive-equal?))
                  <reexported-module>
               else
                  <imported-module>
               end if;
         module-list := add!(module-list,
               make(module-class, import-name: import-name, local-name: local-name,
                    used-library: used-lib, source-token: use-clause));
      end for;
   end for;
   module-list
end method;

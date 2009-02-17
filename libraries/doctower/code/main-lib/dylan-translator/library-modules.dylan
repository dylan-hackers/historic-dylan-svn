module: dylan-translator


define method extract-library (files :: <sequence>)
=> (library-set :: <pair> /* of <library>, <sequence> of <interchange-file-token> */)
   let libs = choose-interchange-definitions(<library-definer-token>, files);
   let library =
         select (libs.size)
            0 =>
               let files = map(source-file, map(token-src-loc, files));
               no-library-in-fileset(filenames: item-string-list(files));
            1 =>
               let token = libs.first;
               make(<library>, source-location: token.token-src-loc,
                    local-name: token.api-name);
            otherwise =>
               let dupes = map(token-src-loc, libs);
               multiple-libraries-in-fileset(defn-locations: item-string-list(dupes));
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
   let library-tokens = map(
         method (files) => (token)
            choose-interchange-definitions(<library-definer-token>, files).first
         end, map(tail, library-sets));

   // Step 1
   
   let dependencies = make(<case-insensitive-string-table>);
   for (lib keyed-by i in libraries)
      // Get used libraries from use clauses.
      let token = library-tokens[i];
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
            (full-dependent-list :: <stretchy-vector>, lib :: <string>)
         => (full-dependent-list :: <stretchy-vector>)
            let used-libs = element(dependencies, lib, default: #[]);
            for (used-lib in used-libs)
               full-dependent-list := library-dependencies(full-dependent-list, used-lib);
            end for;
            add-new!(full-dependent-list, lib, test: case-insensitive-equal?);
         end method;
   
   let full-dependencies = make(<case-insensitive-string-table>);
   for (lib-name in dependencies.key-sequence)
      let full-dependent-list = make(<stretchy-vector>);
      full-dependencies[lib-name] := library-dependencies(full-dependent-list, lib-name);
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

Modules fall into these categories:

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
                 in export option. Instance of <imported-module>.
   
   Dylan-User  - Every library has an internal Dylan-User module that can't be
                 exported from the library. We are not including it, but if it
                 were included, it would be an instance of <internal-module>
                 with pre-defined bindings.

The function operates as follows:
   1. Populate the libraries with exported and internal modules derived from
      export clauses.
   2. Populate the libraries with imported, renamed, and reexported clauses
      listed in use clauses and carried forward from the other libraries.
   3. Populate the libraries with modules mentioned outside of the library
      definition, such as used modules listed in module definitions.
   4. Remove duplicates and sort modules in each library by dependency.

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
      let library-files = library-set.tail;
      let token = choose-interchange-definitions
            (<library-definer-token>, library-files).first;
      let module-tokens = extract-module-tokens(library, library-files);
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
   
   // Step 3
   
   for (library keyed-by i in libraries)
      let mod-clauses = map(namespace-clauses, library-module-tokens[i]);
      let mod-clauses = apply(concatenate, #[], mod-clauses);
      let mod-use-clauses = choose(rcurry(instance?, <use-clause-token>), mod-clauses);
      let unknown-mod-clauses = difference(mod-use-clauses, library.modules,
            test: method (clause :: <use-clause-token>, module :: <module>)
                  => (same? :: <boolean>)
                     case-insensitive-equal?(clause.use-name, module.local-name)
                  end);

      // Assume any used modules that aren't already known are imported from
      // another library.
      local method make-imported-module (clause :: <use-clause-token>)
            => (module :: <imported-module>)
               make(<imported-module>, local-name: clause.use-name,
                    import-name: #f, used-library: #f,
                    source-location: clause.token-src-loc)
            end method;
      let unknown-modules = map(make-imported-module, unknown-mod-clauses);
      library.modules := concatenate!(library.modules, unknown-modules);
   end for;

   // Step 4

   do(merge-modules, libraries);
   do(sort-modules, libraries, library-module-tokens);
end method;


define method extract-module-tokens (library :: <library>, files :: <sequence>)
=> (tokens :: <sequence> /* of <module-definer-token> */)
   let mod-tokens = choose-interchange-definitions(<module-definer-token>, files);
   let modules-by-name = group-elements(mod-tokens,
         test: method (mod-1 :: <module-definer-token>, mod-2 :: <module-definer-token>)
               => (dup? :: <boolean>)
                  case-insensitive-equal?(mod-1.api-name, mod-2.api-name)
               end method);
   let duplicate-modules = choose(method (mods) => (dups?) mods.size > 1 end,
                                  modules-by-name);
   for (duplicate-set in duplicate-modules)
      let defn-locations = map(token-src-loc, duplicate-set);
      duplicate-modules-in-fileset(name: duplicate-set.first.api-name,
            defn-locations: defn-locations.item-string-list)
   end for;
   mod-tokens
end method;


define method modules-from-export-clauses
   (export-clauses :: <sequence>, module-tokens :: <sequence>)
=> (modules :: <sequence>)
   let defined-module-names = map(api-name, module-tokens);

   local method undefined-module? (name :: <string>) => (undefined? :: <boolean>)
            ~member?(name, defined-module-names, test: case-insensitive-equal?)
         end method;

   for (clause in export-clauses)
      let undefined-names = choose(undefined-module?, clause.export-names);
      for (name in undefined-names)
         undefined-module-in-library(location: clause.token-src-loc, name: name);
      end for;
   end for;

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
                 local-name: token.api-name, source-location: token.token-src-loc)
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
      library-exports-not-known(location: use-clause.token-src-loc);
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
                    used-library: used-lib, source-location: use-clause.token-src-loc));
      end for;
   end for;
   module-list
end method;


/**
A module may be exported by multiple export clauses, and modules may be used by
several use clauses independently. This function checks and eliminates duplicates.
**/
define method merge-modules (library :: <library>) => ()
   let new-modules = make(<stretchy-vector>);

   local method has-name? (name :: <string>, module :: <module>)
         => (has-name? :: <boolean>)
            case-insensitive-equal?(name, module.local-name)
         end method,
         
         method better (mod-1 :: false-or(<module>), mod-2 :: false-or(<module>))
         => (mod :: false-or(<module>))
            if (mod-1 = mod-2)
               select (mod-2.object-class)
                  (<exported-module>, <reexported-module>) => mod-2;
                  otherwise => mod-1;
               end select
            end if
         end method;

   iterate extract-next-name (mod-list = library.modules)
      let mod-name = mod-list.first.local-name;
      let (shared-name-modules, other-name-modules) =
            partition(curry(has-name?, mod-name), mod-list);
      let chosen :: false-or(<module>) = reduce1(better, shared-name-modules);

      if (chosen)
         new-modules := add!(new-modules, chosen);
      else
         let sources = map(source-location, shared-name-modules);
         conflicting-modules-in-library(location: library.source-location,
               name: mod-name, defn-locations: sources.item-string-list)
      end if;

      unless (other-name-modules.empty?)
         extract-next-name(other-name-modules);
      end unless;
   end iterate;
   
   library.modules := new-modules;
end method;


/**
Synopsis: Sort modules within a library by dependencies on each other.

The algorithm here is very to that of 'dependent-libraries'. One difference is
that, for libraries, a library outside the scope of a project may be referenced,
but for modules, all modules referenced are in the library.

   1. Give each module a row of a table. Contents of the row is the list of
      modules used by that module. A module imported from another library has
      no dependencies within this library.
   2. Go through each used module list. If a module in the list has a row,
      recursively insert its dependencies' dependencies, then its dependencies,
      then the module.
   3. When finished, each module will have a complete dependency list. Sort the
      modules in ascending order of number of dependencies. Each dependency list
      will be complete and each module in each list will be preceded by its
      dependencies. Therefore, if bindings are created for each module in order,
      the bindings of a used module of a module will always be known for that
      module.
**/
define method sort-modules (library :: <library>, module-tokens :: <sequence>) => ()
   local method has-name? (name :: <string>, token :: <module-definer-token>)
         => (has-name? :: <boolean>)
            case-insensitive-equal?(token.api-name, name)
         end method;

   // Step 1
   
   let dependencies = make(<case-insensitive-string-table>);
   for (mod in library.modules)
      let dependency-list =
            if (instance?(mod, type-union(<internal-module>, <exported-module>)))
               let token = choose(curry(has-name?, mod.local-name),
                                  module-tokens).first;
               let clauses = token.namespace-clauses;
               let use-clauses = choose(rcurry(instance?, <use-clause-token>), clauses);
               remove-duplicates!(map(use-name, use-clauses),
                                  test: case-insensitive-equal?);
            else
               #[]
            end if;
      dependencies[mod.local-name] := dependency-list;
   end for;
   
   // Step 2
   
   local method module-dependencies
            (full-dependent-list :: <stretchy-vector>, mod :: <string>)
         => (full-dependent-list :: <stretchy-vector>)
            for (used-mod in dependencies[mod])
               full-dependent-list := module-dependencies(full-dependent-list, used-mod);
            end for;
            add-new!(full-dependent-list, mod, test: case-insensitive-equal?);
         end method;
   
   let full-dependencies = make(<case-insensitive-string-table>);
   for (mod-name in dependencies.key-sequence)
      let full-dependent-list = make(<stretchy-vector>);
      full-dependencies[mod-name] := module-dependencies(full-dependent-list, mod-name);
   end for;
   
   // Step 3
   
   local method compare-dependency-size (mod1 :: <module>, mod2 :: <module>)
         => (mod1-less? :: <boolean>)
            let mod1-deps = full-dependencies[mod1.local-name].size;
            let mod2-deps = full-dependencies[mod2.local-name].size;
            mod1-deps < mod2-deps;
         end method;

   library.modules := sort!(library.modules, test: compare-dependency-size);
end method;

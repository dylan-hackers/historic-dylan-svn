module: dylan-translator
synopsis: Creates libraries and modules. Does not create bindings.


//
// Circular reference prevention
//


define constant $incomplete-table = make(<case-insensitive-string-table>);


define macro with-incomplete
   {  with-incomplete (?token:expression, ?names)
         ?:body
      end
   }
   => {  let key = ?names;
         $incomplete-table[key] := ?token;
         block ()
            ?body
         cleanup
            remove-key!($incomplete-table, key);
         end block }
names:
   { ?:name } => { ?name }
   { ?first:name, ?second:name } => { format-to-string("%s:%s", ?first, ?second) }
end macro;


define method check-library-not-incomplete
   (library-name :: <string>, reason-token :: <token>)
=> ()
   let incomplete = element($incomplete-table, library-name, default: #f);
   when (incomplete)
      let locations = vector(incomplete.token-src-loc, reason-token.token-src-loc);
      circular-definition(defn-locations: locations.item-string-list)
   end when;
end method;


define method check-module-not-incomplete
   (library-name :: <string>, module-name :: <string>, reason-token :: <token>)
=> ()
   let key-name = format-to-string("%s:%s", library-name, module-name);
   let incomplete = element($incomplete-table, key-name, default: #f);
   when (incomplete)
      let locations = vector(incomplete.token-src-loc, reason-token.token-src-loc);
      circular-definition(defn-locations: locations.item-string-list)
   end when;
end method;


//
// Utility
//


define method library-module (library :: <library>, module-name :: <string>)
=> (module :: false-or(<module>))
   find-element(library.modules,
         method (mod :: <module>) => (match?)
            case-insensitive-equal?(mod.local-name, module-name)
         end, failure: #f)
end method;


define method namespace-has-local-name? (module :: <module>, name :: <string>)
=> (match? :: <boolean>)
   case-insensitive-equal?(module.local-name, name)
end method;
         

//
// Libraries
//


define method find/make-library
   (library-sets :: <table>, reason-token :: <token>, library-name :: false-or(<string>))
=> (library :: <library>)
   if (library-name)
      check-library-not-incomplete(library-name, reason-token);
      let library-set = element(library-sets, library-name, default: #f);
      let library = library-set & library-set.libset-library;
      library | make-library(library-sets, reason-token, library-name)
   else
      make(<unknown-library>, local-name: #f,
           source-location: reason-token.token-src-loc);
   end if;
end method;


define method make-library
   (library-sets :: <table>, reason-token :: <token>, library-name :: <string>)
=> (library :: <library>)
   // We may not have a library set, but if we have a library token we will.
   let library-set = element(library-sets, library-name, default: #f);
   let library-token = library-set & library-set.libset-library-token;
   with-incomplete (library-token | reason-token, library-name)
      let library = library-from-token(library-sets, reason-token, library-name,
                                       library-token);
      unless (library-set)
         library-set := make(<library-set>, library: library);
         library-sets[library-name] := library-set;
      end unless;
      library-set.libset-library := library;
   end with-incomplete;
end method;


define method library-from-token
   (library-sets :: <table>, reason-token :: <token>, library-name :: <string>,
    library-token == #f)
=> (library :: <library>)
   make(<unknown-library>, local-name: library-name,
        source-location: reason-token.token-src-loc)
end method;


define method library-from-token
   (library-sets :: <table>, reason-token :: <token>, library-name :: <string>,
    library-token :: <library-definer-token>)
=> (library :: <library>)
   // Library-set is always present because it will be created for the token.
   let library-set = library-sets[library-name];
   let library = make(<known-library>, local-name: library-name,
                      source-location: library-token.token-src-loc);
   for (clause in library-token.namespace-clauses)
      find/make-modules-from-clause(library-sets, library, clause);
   end for;
   let module-tokens = library-set.libset-module-tokens;
   for (module-token keyed-by module-name in module-tokens)
      find/make-module(library-sets, module-token, library, module-name, infer: #f)
   end for;
   library
end method;


define method check-library-imports-exports
   (library-sets :: <table>, library :: <unknown-library>)
=> ()
end method;


define method check-library-imports-exports
   (library-sets :: <table>, library :: <known-library>)
=> ()
   let token :: <library-definer-token> =
         library-sets[library.local-name].libset-library-token;
   let use-clauses = choose(rcurry(instance?, <use-clause-token>),
                            token.namespace-clauses);
   do(curry(find/make-modules-from-clause, library-sets, library), use-clauses);
end method;


define method find/make-modules-from-clause
   (library-sets :: <table>, library :: <library>, clause :: <export-clause-token>)
=> (modules :: <sequence>)
   map(method (name :: <string>) => (module :: <module>)
          find/make-module(library-sets, clause, library, name, exported: #t, infer: #f)
       end, clause.export-names)
end method;


define method find/make-modules-from-clause
   (library-sets :: <table>, library :: <library>, clause :: <use-clause-token>)
=> (modules :: <sequence>)
   let import-options :: type-union(<sequence> /* of <renaming-token> */, singleton(#"all"))
         = clause.use-imports | #"all";
   let rename-options :: <sequence> /* of <renaming-token> */
         = clause.use-renamings | #[];
   let exclude-options :: <sequence> /* of <string> */
         = clause.use-exclusions | #[];
   let export-options :: type-union(<sequence> /* of <string> */, singleton(#"all"))
         = clause.use-exports | #[];
   let prefix-option :: false-or(<string>)
         = clause.use-prefix;
   
   let import-and-rename-options :: <sequence> /* of <renaming-token> */ =
         concatenate(if (import-options = #"all") #[] else import-options end,
                     rename-options);
   
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
            let renaming = find-element(import-and-rename-options,
                                        curry(local-name-of-renaming?, export-name),
                                        default: #f);
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

   // Ensure used library has any explicitly-named modules.
   
   let used-library = find/make-library(library-sets, clause, clause.use-name);

   let used-library-import-names = map(token-import-name, import-and-rename-options);
   let used-library-export-names =
         if (export-options ~= #"all")
            map(import-name-for-exported-module, export-options);
         else
            #[];
         end if;
   let used-library-names = concatenate(used-library-import-names, 
         used-library-export-names, exclude-options);
   for (name in used-library-names)
      find/make-module(library-sets, clause, used-library, name, exported: #t);
   end for;
   
   // Get used library's imported modules.
   
   let all-modules = (used-library & used-library.modules) | #[];
   let all-modules = choose(exported?, all-modules);
   let non-excluded-modules = difference(all-modules, exclude-options,
                                         test: namespace-has-local-name?);
   let imported-modules =
         if (import-options = #"all")
            non-excluded-modules
         else
            let imported-names = map(token-import-name, import-options);
            intersection(non-excluded-modules, imported-names,
                         test: namespace-has-local-name?)
         end if;
                  
   // Determine local name or names (if multiple import/rename options) of
   // each imported module.

   let modules-import-name :: <sequence> /* of <string> */
         = map(local-name, imported-modules);
   let modules-local-names :: <sequence> /* of <sequence> of <string> */
         = map(local-names-for-imported-module, modules-import-name);

   // Make modules in this library.

   let module-list = make(<stretchy-vector>);
   for (import-name in modules-import-name, local-names in modules-local-names)
      for (local-name in local-names)
         let exported? = (export-options = #"all") |
               member?(local-name, export-options, test: case-insensitive-equal?);
         let new-module = find/make-module(library-sets, clause, library, local-name,
               used-library: used-library, import-name: import-name, exported: exported?);
         module-list := add!(module-list, new-module);
      end for;
   end for;
   
   // Note unknown exports from used library.
   
   when (import-options = #"all" & export-options = #"all")
      let sources = union(library.unknown-reexport-sources,
                          used-library.unknown-reexport-sources, test: \=);
      when (instance?(used-library, <unknown-library>))
         sources := add-new!(sources, used-library, test: \=);
      end when;
      library.unknown-reexport-sources := sources;
   end when;
   
   module-list
end method;


//
// Modules
//


define method find/make-module
   (library-sets :: <table>, reason-token :: <token>, library :: <library>,
    module-name :: <string>, #rest keys,
    #key exported: module-exported? :: <boolean> = #f,
         infer: inferred? :: <boolean> = #t,
         import-name :: false-or(<string>) = #f,
         used-library :: false-or(<library>) = #f)
=> (module :: <module>)
   let library-name = library.local-name | "";
   check-module-not-incomplete(library-name, module-name, reason-token);
   // If the requested module already exists, the new module will be merged into it.
   apply(make-module, library-sets, reason-token, library, module-name, keys);
end method;


define method make-module
   (library-sets :: <table>, reason-token :: <token>, library :: <library>,
    module-name :: <string>, #key exported: module-exported? :: <boolean> = #f,
    import-name :: false-or(<string>) = #f, used-library :: false-or(<library>) = #f,
    infer: inferred? :: <boolean> = #t)
=> (module :: <module>)
   // We may not have a library set, but if we have a module token we will.
   let library-name = library.local-name | "";
   let library-set = element(library-sets, library-name, default: #f);
   let module-tokens = library-set & library-set.libset-module-tokens;
   let module-token = module-tokens & element(module-tokens, module-name, default: #f);
   with-incomplete (module-token | reason-token, library-name, module-name)
      let module = module-from-token(library-sets, reason-token,
            library, module-name, used-library, import-name, module-token,
            module-exported?, inferred?);
      merge-module(library, module, module-token.true?);
   end with-incomplete;
end method;


/**
Synopsis: Add a module to a library's module list, merging with duplicate modules.

A module may be exported by multiple export clauses, and modules may be used by
several use clauses independently. Additionally, the same module may be created
in a number of different ways. This function resolves all those things.

 * A <local-module> supersedes an <imported-module> from an anonymous library
   because the latter represents a binding in an unknown library, which may in
   fact be the local library.
 * An <imported-module> from a known library supersedes an <imported-module>
   from an anonymous library.
 * A module created directly from a module definition (a "canonical" module)
   supersedes a non-canonical module, to ensure the <module>'s 'source-location'
   is correct.
 * An exported module supersedes an unexported module.
 * 'Unknown-reexport-sources' are merged.
**/
define method merge-module
   (library :: <library>, new-module :: <module>, canonical? :: <boolean>)
=> (merged-module :: <module>)
   let module-name = new-module.local-name;
   let existing-module = library-module(library, module-name);
   
   case
      ~existing-module =>
         library.modules := add!(library.modules, new-module);
         new-module;
      
      existing-module ~= new-module =>
         let conflicts = vector(existing-module.source-location,
                                new-module.source-location);
         conflicting-modules-in-library(location: library.source-location,
               name: module-name, defn-locations: conflicts.item-string-list);
         
      otherwise =>
         let merged-source-location = if (canonical?) new-module else existing-module end
               .source-location;
         let merged-export = new-module.exported? | existing-module.exported?;
         let new-imported? = instance?(new-module, <imported-module>);
         let existing-imported? = instance?(new-module, <imported-module>);
         let (merged-class, merged-used-library, merged-import-name) =
               case
                  existing-imported? & existing-module.used-library.anonymous? =>
                     values(new-module.object-class,
                            new-imported? & new-module.used-library,
                            new-imported? & new-module.import-name);
                  otherwise =>
                     values(existing-module.object-class,
                            existing-imported? & existing-module.used-library,
                            existing-imported? & existing-module.import-name);
               end case;
         let merged-reexport-sources = union(new-module.unknown-reexport-sources,
                                             existing-module.unknown-reexport-sources,
                                             test: \=);
         // TODO: Need any special procedures to merge the above bindings?

         let merged-module =
               make(merged-class, local-name: module-name, exported: merged-export,
                    used-library: merged-used-library, import-name: merged-import-name,
                    source-location: merged-source-location);
         merged-module.unknown-reexport-sources := merged-reexport-sources;
         
         library.modules := replace-elements!(library.modules,
               curry(\==, existing-module), always(merged-module));
         merged-module;
   end case
end method;


/**
This table describes what kind of module results from different arguments.

module-token  import info  inferred?  class or result
------------  -----------  ---------  -----------------------------------
YES           YES          YES/NO     conflicting-modules-in-library
                                      or <local-module>
YES           NO           YES/NO     <local-module>
NO            YES          YES/NO     <imported-module>
NO            NO           NO         undefined-module-in-library
NO            NO           YES        <local-module> or <imported-module>
------------  -----------  ---------  -----------------------------------

The 'module-token' and import info arguments may both be present under the
the following scenarios.

1. A module may be required before it is created from its definition. In this
   circumstance, the module will be assumed to be from an anonymous library.
   Instead, the module should be created from its definition.

The 'module-token' and import info arguments may be legitimately missing under
the following scenarios.

1. Library A may import a module from library B that library B re-exports from
   an inferred library C but that library B does not explicitly name. While
   constructing library B, the module in question will not have a module
   definition because it is imported, but since it is not explicitly named its
   import information will not be known.
   
   The use clause of library A will cause 'module-from-token' to be called with
   a 'library' of B. The module created should be an <imported-module> in
   library B. The module's used-library is library C, but that is not known and
   will be #f.

2. Library A may import a module from an inferred library B. Library B will not
   have a module definition for the module. This occurs in the use clause of
   library A.

   The use clause of library A will cause 'module-from-token' to be called with
   a 'library' of B. The module created should be a <local-module> in library B.

3. Module A may use module B that was imported from an unknown library but not
   explicitly named. Module B will not have been created during the initial
   construction of the library.
   
   The use clause of module A will cause 'module-from-token' to be called with a
   'library' of module A's library. The module created should be an
   <imported-module> in library A. The module's used-library is the unknown
   library, but that is not known and will be #f.
**/
define generic module-from-token
   (library-sets :: <table>, reason-token :: <token>,
    library :: <library>, module-name :: <string>,
    used-library :: false-or(<library>), import-name :: false-or(<string>),
    module-token :: false-or(<module-definer-token>),
    exported? :: <boolean>, inferred? :: <boolean>)
=> (module :: <module>);
   

/**
Called for imported or inferred-to-exist module that has a definition.
Signal error or create local module from definition.
**/
define method module-from-token
   (library-sets :: <table>, reason-token :: <token>, library :: <library>,
    module-name :: <string>, used-library :: <library>, import-name :: <string>,
    module-token :: <module-definer-token>, exported? :: <boolean>,
    inferred? :: <boolean>)
=> (module :: <module>)
   if (used-library.anonymous?)
      module-from-token(library-sets, reason-token, library, module-name, #f, #f,
                        module-token, exported?, inferred?)
   else
      let locations = vector(reason-token.token-src-loc, module-token.token-src-loc);
      conflicting-modules-in-library(location: library.source-location,
            name: module-name, defn-locations: locations.item-string-list);
   end if;
end method;


/** Called to create local module from definition. **/
define method module-from-token
   (library-sets :: <table>, reason-token :: <token>, library :: <library>,
    module-name :: <string>, used-library :: false-or(<library>),
    import-name :: false-or(<string>), module-token :: <module-definer-token>,
    exported? :: <boolean>, inferred? :: <boolean>)
=> (module :: <local-module>)
   let module = make(<local-module>, local-name: module-name, exported: exported?,
                     source-location: module-token.token-src-loc);
   let use-clauses = choose(rcurry(instance?, <use-clause-token>),
                            module-token.namespace-clauses);
   do(method (clause :: <use-clause-token>) => (mod :: <module>)
         let used-library = find/make-library(library-sets, clause, #f);
         find/make-module(library-sets, clause, library, clause.use-name,
                          used-library: used-library, import-name: clause.use-name);
      end method, use-clauses);
   module
end method;


/** Called to create imported or inferred-to-exist module. **/
define method module-from-token
   (library-sets :: <table>, reason-token :: <token>, library :: <library>,
    module-name :: <string>, used-library :: <library>, import-name :: <string>,
    module-token == #f, exported? :: <boolean>, inferred? :: <boolean>)
=> (module :: <imported-module>)
   make(<imported-module>, local-name: module-name, exported: exported?,
        import-name: import-name, used-library: used-library,
        source-location: reason-token.token-src-loc)
end method;


/** Called to create local module without definition. Signal error. **/
define method module-from-token
   (library-sets :: <table>, reason-token :: <token>, library :: <library>,
    module-name :: <string>, used-library == #f, import-name == #f,
    module-token == #f, exported? :: <boolean>, inferred? == #f)
=> (module :: <module>)
   undefined-module-in-library(location: reason-token.token-src-loc,
         name: module-name);
end method;


/**
Called to create inferred module in known library. Assume requested module is
imported from somewhere else.
**/
define method module-from-token
   (library-sets :: <table>, reason-token :: <token>, library :: <known-library>,
    module-name :: <string>, used-library == #f, import-name == #f,
    module-token == #f, exported? :: <boolean>, inferred? == #t)
=> (module :: <imported-module>)
   let used-library = find/make-library(library-sets, reason-token, #f);
   let import-name = module-name;
   find/make-module(library-sets, reason-token, used-library, import-name);
   make(<imported-module>, local-name: module-name, exported: exported?,
        import-name: import-name, used-library: used-library,
        source-location: reason-token.token-src-loc)
end method;


/**
Called to create inferred module in unknown library. Assume requested module is
local to unknown library.
**/
define method module-from-token
   (library-sets :: <table>, reason-token :: <token>, library :: <unknown-library>,
    module-name :: <string>, used-library == #f, import-name == #f,
    module-token == #f, exported? :: <boolean>, inferred? == #t)
=> (module :: <local-module>)
   make(<local-module>, local-name: module-name, exported: exported?,
        source-location: reason-token.token-src-loc)
end method;

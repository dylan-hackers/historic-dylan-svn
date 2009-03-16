module: dylan-translator
synopsis: Common code dealing with library and module representations.


//
// Library and module annotations
//


/**
Library and module annotations track dependencies, but they track them by way of
<use-clause-token>s rather than simple names so that source locations are
available.
**/
define class <namespace-annotation> (<object>)
   slot annot-dependencies :: <sequence> /* of <use-clause-token> */ = #[];
end class;


//
// Use clause processing
//


/**
Synopsis: Use clause processing.

--- Arguments: ---

clause -
   The <use-clause-token> being processed.

foreign-items -
   An instance of <sequence>. The modules or bindings contained by the used
   library or module (respectively). This list should include all the modules or
   bindings.
   
make-stray -
   An instance of <function> taking a <string> argument. The function is called
   to create a new stray module or binding in the used library or module; the
   argument is the name of the new module or binding to create.
   
   The function is needed during the 'make-inferences-and-imports' stage of
   processing. Later in the stage, the used library or module will be visited
   and the module/binding's stray status will be resolved if possible.
   
   During the 'make-all-imports' stage, no new modules or bindings will need to
   be created in the used library or module and the function will not be called.

make-or-update-imported -
   An instance of <function> taking three <string> arguments: a local name, an
   import name, and an export flag. The function is called to make an
   <imported-module> or <imported-binding> corresponding to a module or binding
   from the used library or module, or to update a stray module or binding with
   its actual origin.

note-unknown-reexport-source -
   An instance of <function> taking no arguments. This function is called if the
   use clause imports and reexports all modules or bindings from the used
   library or module. When the library or module is inferred, it may include
   other modules or bindings that are not known; if so, the function should note
   the possibility.
   
   This function need not do anything during the 'make-all-imports' stage, since
   any eligible libraries or modules will already have been noted during the
   'make-inferences-and-imports' stage.
**/
define method infer-and-import-clause
   (clause :: <use-clause-token>, foreign-items :: <sequence>,
    make-stray :: <function>, make-or-update-imported :: <function>,
    note-unknown-reexport-source :: <function>)
=> ()
   
   // Interpret use clause options.

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
         
         method local-names-for-imported-name (import-name :: <string>)
         => (local-names :: <sequence> /* of <string> */)
            let renamings = choose(curry(import-name-of-renaming?, import-name),
                                   import-and-rename-options);
            case
               ~renamings.empty? => map(local-name-of-renaming, renamings);
               prefix-option => vector(concatenate(prefix-option, import-name));
               otherwise => vector(import-name);
            end case
         end method,
         
         method import-name-for-exported-name (export-name :: <string>)
         => (import-name :: <string>)
            let renaming = find-element(import-and-rename-options,
                                        curry(local-name-of-renaming?, export-name),
                                        failure: #f);
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

   // Get all explicit names.
   
   let imported-names = map(token-import-name, import-and-rename-options);
   let foreign-exported-names =
         if (export-options ~= #"all")
            map(import-name-for-exported-name, export-options);
         else
            #[];
         end if;
   let foreign-explicit-names = concatenate
         (imported-names, foreign-exported-names, exclude-options);
   
   // Create stray items for undefined explicit names.
   
   let foreign-defined-names = map(local-name, foreign-items);
   let foreign-undefined-names = difference
         (foreign-explicit-names, foreign-defined-names, test: case-insensitive-equal?);
   do(make-stray, foreign-undefined-names);
   
   // Get foreign importable and imported names.
   
   let importable-foreign-items = choose(exported?, foreign-items);
   let non-excluded-foreign-items = difference
         (importable-foreign-items, exclude-options, test: has-local-name?);
   let imported-foreign-items =
         if (import-options = #"all")
            non-excluded-foreign-items
         else
            let imported-names = map(token-import-name, import-options);
            intersection(non-excluded-foreign-items, imported-names,
                         test: has-local-name?)
         end if;
                  
   // Determine local name or names (if multiple import/rename options) of
   // each imported item.

   let imported-import-names :: <sequence> /* of <string> */
         = map(local-name, imported-foreign-items);
   let imported-local-names :: <sequence> /* of <sequence> of <string> */
         = map(local-names-for-imported-name, imported-import-names);

   // Make items, or update previously inferred items.

   for (import-name in imported-import-names, local-names in imported-local-names)
      for (local-name in local-names)
         let export = (export-options = #"all") |
               member?(local-name, export-options, test: case-insensitive-equal?);
         make-or-update-imported(local-name, import-name, export);
      end for;
   end for;
   
   // Note unknown exports from used library.
   
   when (import-options = #"all" & export-options = #"all")
      note-unknown-reexport-source()
   end when;
   
end method;



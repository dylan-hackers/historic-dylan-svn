module: dylan-translator
synopsis: Contains code in charge of building representations of Dylan APIs.


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
   verbose-log("Cataloging definitions");
   let annotations = make-annotations(file-sets);
   make-api-representations(annotations);
   order-by-dependency(annotations);
   make-inferences-and-imports(annotations);
   order-by-dependency(annotations);
   make-all-imports-and-defns(annotations);

   /**/
   log-object("Libraries", map(annot-library, annotations.element-sequence));
   for (lib-annot keyed-by lib-name in annotations)
      log-object(format-to-string("Modules in %s", lib-name),
                 map(annot-module, lib-annot.annot-modules.element-sequence))
   end for;
   for (lib-annot keyed-by lib-name in annotations)
      for (mod-annot keyed-by mod-name in lib-annot.annot-modules)
         log-object(format-to-string("Bindings in %s:%s", lib-name, mod-name),
                    as(<simple-vector>, mod-annot.annot-module.bindings))
      end for;
   end for;
   /**/
   
   choose(rcurry(instance?, <known-library>),
          map(annot-library, annotations.element-sequence));
end method;


//
// Annotation creation
//


/**
Synopsis: Cache useful information about libraries and modules.
**/
define method make-annotations (file-sets :: <sequence>)
=> (annotations :: <skip-list>)
   let annotations = make(<case-insensitive-skip-list>);
   do(curry(make-annotations-from-files, annotations), file-sets);
   annotations
end method;


define method make-annotations-from-files
   (annotations :: <skip-list>, files :: <sequence>)
=> (library-annot :: <library-annot>)
   
   // Library token
   
   let library-tokens =
         choose-interchange-definitions(<library-definer-token>, files);
   let (library, library-annot) = 
         select (library-tokens.size)
            0 =>
               let files = map(source-file, map(token-src-loc, files));
               no-library-in-fileset(filenames: item-string-list(files));
            1 =>
               let token = library-tokens.first;
               make-defined-library(annotations, token, local-name: token.api-name,
                                    source-location: token.token-src-loc);
            otherwise =>
               let dupes = map(token-src-loc, library-tokens);
               multiple-libraries-in-fileset(defn-locations: item-string-list(dupes));
         end select;
   
   // Module tokens
   
   let module-tokens = choose-interchange-definitions(<module-definer-token>, files);
   for (token :: <module-definer-token> in module-tokens)
      let existing-defn = element(library-annot.annot-modules, token.api-name,
                                  default: #f);
      when (existing-defn)
         let locations = vector(existing-defn.token-src-loc, token.token-src-loc);
         duplicate-modules-in-fileset(name: token.api-name,
               defn-locations: locations.item-string-list)
      end when;
      make-defined-module(library-annot.annot-modules, token, library,
                          local-name: token.api-name,
                          source-location: token.token-src-loc);
   end for;
   
   // Module files & definitions

   for (file in files)
      let header = file.module-header;
      let file-module = header.hdr-value;
      unless (case-insensitive-equal?(file-module, "dylan-user"))
         let module-annot = element(library-annot.annot-modules, file-module,
                                    default: #f);
         unless (module-annot)
            undefined-module-for-interchange-file(location: header.token-src-loc,
                                                  name: header.hdr-value)
         end unless;
         module-annot.annot-definitions := concatenate!(module-annot.annot-definitions,
               choose-interchange-definitions(<non-namespace-definition-token>,
                                              vector(file)));
      end unless;
   end for;
   
   library-annot
end method;


//
// Populating libraries and modules
//


/**
Synopsis: Create representations of all defined APIs, not including imports.
**/
define method make-api-representations (annotations :: <skip-list>) => ()
   for (lib-annot in annotations)
      let lib = lib-annot.annot-library;
      let deps = dependencies-from-token(lib-annot.annot-token);
      lib-annot.annot-dependencies := deps;

      for (mod-annot in lib-annot.annot-modules)
         let mod = mod-annot.annot-module;
         let deps = dependencies-from-token(mod-annot.annot-token);
         mod-annot.annot-dependencies := deps;
         make-created-from-token(lib, mod, mod-annot.annot-token);
      end for;
      flag-and-check-exports-exist(lib, lib-annot.annot-token);
   end for;
end method;


define method make-created-from-token
   (library :: <library>, module :: <module>, token :: <module-definer-token>)
=> ()
   let create-clauses = choose(rcurry(instance?, <create-clause-token>),
                               token.namespace-clauses);
   let created-names = #[];
   for (clause in create-clauses)
      let names = apply(concatenate, #[], map(create-names, create-clauses));
      let names = remove-duplicates!(names, test: case-insensitive-equal?);
      let new-names = difference(names, created-names, test: case-insensitive-equal?);
      do(curry(make-created-binding, library, module,
               source-location:, clause.token-src-loc, local-name:),
         new-names);
      created-names := concatenate!(created-names, new-names);
   end for;
end method;


//
// Inferences and imports
//


/**
Synopsis: Create representations of imported and unknown APIs.

Representations are passed to this method in order of dependency, libraries and
modules with no prerequisites first. This method starts at the other end, with
the libraries and modules depending on the greatest number of other libraries or
modules, and traces back to the start to infer any unknown APIs and match them
to defined APIs in prerequisite libraries or modules.
**/
define method make-inferences-and-imports (annotations :: <skip-list>) => ()
   for (lib-annot in annotations using backward-iteration-protocol)
      // Infer-and-import-for-library may create new modules.
      // If it does so, we need to redo dependencies so the inner loop is right.
      infer-and-import-for-library(annotations, lib-annot.annot-library);
      order-list-by-dependency(lib-annot.annot-modules);
      for (mod-annot in lib-annot.annot-modules using backward-iteration-protocol)
         infer-and-import-for-module(lib-annot.annot-modules,
               lib-annot.annot-library, mod-annot.annot-module);
      end for;
   end for;
end method;


//
// Carrying exports
//


/**
Synopsis: Propogate APIs to dependent libraries and modules.

Once all stray APIs and inferred libraries/modules are known, this method goes
back and makes sure that all dependent libraries and modules that use all APIs
of a library or module are updated with any newly-added APIs.

It also adds definitions to all bindings and ensures that imported bindings are
not mentioned in "export" clauses.
**/
define method make-all-imports-and-defns (annotations :: <skip-list>) => ()
   for (lib-annot in annotations)
      // Import-all-for-library may create new modules.
      // If it does so, we need to redo dependencies so the inner loop is right.
      let lib = lib-annot.annot-library;
      import-all-for-library(annotations, lib);
      order-list-by-dependency(lib-annot.annot-modules);
      for (mod-annot in lib-annot.annot-modules)
         let mod = mod-annot.annot-module;
         define-and-import-all-for-module(lib-annot.annot-modules, lib, mod);
         flag-and-check-exports-exist(mod, mod-annot.annot-token);
         check-exports-not-imported(mod, mod-annot.annot-token);
      end for;
   end for;
end method;


//
// Exports
//


define method flag-and-check-exports-exist
   (rep :: type-union(<library>, <module>), token == #f)
=> ()
end method;


define method flag-and-check-exports-exist
   (library :: <library>, token :: <library-definer-token>)
=> ()
   check-exported-items-exist(library.modules, token, no-definition-for-modules)
end method;


define method flag-and-check-exports-exist
   (module :: <module>, token :: <module-definer-token>)
=> ()
   check-exported-items-exist(module.bindings, token, no-definition-for-bindings)
end method;


define method check-exported-items-exist
   (items :: <sequence>, token :: <definition-token>, error :: <function>)
=> ()
   local method item-for-name (name :: <string>) => (item)
            find-element(items, rcurry(has-local-name?, name))
         end method;
         
   let export-clauses = choose(rcurry(instance?, <export-clause-token>),
                               token.namespace-clauses);
   let exported-names = #[];
   for (clause in export-clauses)
      let names = apply(concatenate, #[], map(export-names, export-clauses));
      let names = remove-duplicates!(names, test: case-insensitive-equal?);
      let new-names = difference(names, exported-names, test: case-insensitive-equal?);
      let items = map(item-for-name, new-names);
      let missing-names = choose-by(false?, items, new-names);
      unless (missing-names.empty?)
         let quoted-names = map(curry(format-to-string, "\"%s\""), missing-names);
         error(location: clause.token-src-loc, names: quoted-names.item-string-list);
      end unless;
      do(curry(exported?-setter, #t), choose(true?, items));
      exported-names := concatenate!(exported-names, new-names);
   end for;
end method;


//
// Dependencies
//


define method order-by-dependency (annotations :: <skip-list>) => ()
   order-list-by-dependency(annotations);
   for (lib-annot in annotations)
      order-list-by-dependency(lib-annot.annot-modules)
   end for;
end method;


define method order-list-by-dependency (annotations :: <skip-list>) => ()

   // Get complete dependency list of every element.

   let status = make(<case-insensitive-skip-list>);

   local method full-dependencies (name :: <string>, loc :: <source-location>)
         => (full-dependencies :: <sequence>)
            select (element(status, name, default: #"not done"))
               #"not done" =>
                  let annot = element(annotations, name, default: #f);
                  if (annot)
                     status[name] := loc;
                     for (use-clause in annot.annot-dependencies.copy-sequence)
                        let used-annot = element(annotations, use-clause.use-name,
                                                 default: #f);
                        if (used-annot)
                           annot.annot-dependencies := concatenate!
                                 (annot.annot-dependencies,
                                  full-dependencies(use-clause.use-name,
                                                    use-clause.token-src-loc));
                        end if;
                     end for;
                     status[name] := #"done";
                     annot.annot-dependencies := remove-duplicates!
                           (annot.annot-dependencies, test: same-use-name?);
                  else
                     #[];
                  end if;
               #"done" =>
                  annotations[name].annot-dependencies;
               otherwise =>
                  let in-prog-loc = choose(rcurry(instance?, <source-location>),
                                           status.element-sequence);
                  in-prog-loc := add-new!(in-prog-loc, loc, test: \=);
                  circular-definition(name: name,
                        defn-locations: in-prog-loc.item-string-list);
            end select;
         end method;

   for (annot keyed-by name in annotations)
      let loc =
            case
               annot.annot-token => annot.annot-token.token-src-loc;
               otherwise => annot.annot-representation.source-location;
            end case;
      annot.annot-dependencies := full-dependencies(name, loc);
   end for;
   
   // Sort by number of dependencies.
   
   local method compare-dependency-size
            (annot1 :: <namespace-annotation>, annot2 :: <namespace-annotation>)
         => (annot1-less? :: <boolean>)
            let annot1-deps = annot1.annot-dependencies.size;
            let annot2-deps = annot2.annot-dependencies.size;
            annot1-deps < annot2-deps;
         end method;

   annotations.element-sequence :=
         sort(annotations.element-sequence, test: compare-dependency-size);
end method;


define method dependencies-from-token
   (token :: type-union(<library-definer-token>, <module-definer-token>))
=> (dependencies :: <sequence> /* of <use-clause-token> */)
   let clauses = choose(rcurry(instance?, <use-clause-token>), token.namespace-clauses);
   remove-duplicates(clauses, test: same-use-name?)
end method;


//
// Interchange file data extraction
//


define constant <non-namespace-definition-token> =
      type-union(<class-definer-token>, <constant-definer-token>,
                 <function-definer-token>, <generic-definer-token>,
                 <method-definer-token>, <variable-definer-token>,
                 <macro-definer-token>);


define method choose-interchange-definitions
   (type :: <type>, ichange-tokens :: <sequence> /* of <interchange-file-token> */)
=> (seq :: <sequence> /* of type */)
   let source-records = choose(true?, map(source-record, ichange-tokens));
   let source-defns = map(token-definitions, source-records);
   let defns = apply(concatenate, #[], source-defns);
   choose(rcurry(instance?, type), defns);
end method;


define method module-header (file :: <interchange-file-token>)
=> (header :: <header-token>)
   let module-headers = choose(
         method (hdr :: <header-token>) => (mod-hdr?)
            case-insensitive-equal?(hdr.hdr-keyword, "Module")
         end, file.headers);

   when (module-headers.empty?)
      no-header-in-interchange-file(header: "Module:",
            location: file.token-src-loc.source-file);
   end when;

   let header = module-headers.first;
   when (header.hdr-value.empty?)
      empty-header-in-interchange-file(header: "Module:",
            location: header.token-src-loc);
   end when;

   header
end method;


//
// Convenience
//


define method same-api-name? (token1 :: <definition-token>, token2 :: <definition-token>)
=> (same? :: <boolean>)
   case-insensitive-equal?(token1.api-name, token2.api-name)
end method;


define method same-use-name? (token1 :: <use-clause-token>, token2 :: <use-clause-token>)
=> (same? :: <boolean>)
   case-insensitive-equal?(token1.use-name, token2.use-name)
end method;


define method has-local-name? (item :: <object>, name :: <string>)
=> (has-name? :: <boolean>)
   case-insensitive-equal?(item.local-name, name)
end method;


define method has-same-local-name? (item1 :: <object>, item2 :: <object>)
=> (same-name? :: <boolean>)
   case-insensitive-equal?(item1.local-name, item2.local-name)
end method;

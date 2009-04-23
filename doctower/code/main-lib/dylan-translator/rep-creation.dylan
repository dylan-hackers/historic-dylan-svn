module: dylan-translator
synopsis: Contains code in charge of building representations of Dylan APIs.


/**
Synopsis: Returns representation of Dylan libraries, modules, definitions, etc.

Cataloging definitions is a two-pass process. At the start of the first pass,
only explicitly-declared API elements exist.

Each library has a set of modules, and each module has a set of bindings.
Instances of the module and binding classes may be different but refer to the
same conceptual module or binding because of scoped renaming. In contrast, an
each conceptual definition has one and only one instance of a definition class.

During the first pass, conceptual modules and bindings are replicated in other
libraries and modules that use them, and definitions are merged. Additionally,
elements mentioned in the project but not defined in it are inferred to exist.
At the end of this pass, different instances of the same conceptual module or
binding may contain different bindings or definitions. This is because, while
two libraries or modules may both use the same conceptual module, they may use
different parts of the module so that different bindings or definitions are
propogated to corresponding instances of the module or binding class.

During the second pass, these differences are resolved so that all instances of
the same conceptual module or binding contain the same bindings or definition.

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
   prepare-representations(annotations);
   order-by-dependency(annotations);

   // Pass 1
   make-inferences-and-imports(annotations);
   order-by-dependency(annotations);
   make-all-imports-and-defns(annotations);
   order-by-dependency(annotations);
   
   // Pass 2
   make-inferences-and-imports(annotations);
   order-by-dependency(annotations);
   make-all-imports-and-defns(annotations);
   order-by-dependency(annotations);
   
   // TODO: Decide what I can filter out of this list. Inferred modules? Strays?
    map(annot-library, annotations.element-sequence);
end method;


//
// Annotation creation
//


/**
Synopsis: Create defined libraries, modules, and bindings, and cache useful
information.
**/
define method make-annotations (file-sets :: <sequence>)
=> (annotations :: <skip-list>)
   let annotations = make(<case-insensitive-skip-list>, size: 10);
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
               multiple-libraries-in-fileset(location: dupes.first,
                     defn-locations: item-string-list(dupes));
         end select;
   
   // Module tokens
   
   let module-tokens = choose-interchange-definitions(<module-definer-token>, files);
   for (token :: <module-definer-token> in module-tokens)
      let existing-defn = element(library-annot.annot-modules, token.api-name,
                                  default: #f);
      when (existing-defn)
         let locations = vector(existing-defn.token-src-loc, token.token-src-loc);
         duplicate-modules-in-fileset(location: locations.first, name: token.api-name,
               defn-locations: locations.item-string-list)
      end when;
      make-defined-module(annotations, token, library,
                          local-name: token.api-name,
                          source-location: token.token-src-loc);
   end for;
   
   // Module files & definition tokens

   for (file in files)
      let header = file.module-header;
      let file-module = header.hdr-value;
      unless (case-insensitive-equal?(file-module, "dylan-user"))

         // Ensure module exists.
         let module-annot = element(library-annot.annot-modules, file-module,
                                    default: #f);
         unless (module-annot)
            undefined-module-for-interchange-file(location: header.token-src-loc,
                                                  name: header.hdr-value)
         end unless;
         
         // Create definitions and bindings.
         let defn-tokens = choose-interchange-definitions
               (<non-namespace-definition-token>, vector(file));
         make-bindings-from-definitions
               (annotations, library, module-annot.annot-module, defn-tokens);
      end unless;
   end for;
   
   library-annot
end method;


//
// Preparing libraries and modules
//


/**
Synopsis: Create namespace dependencies and check exports.
**/
define method prepare-representations (annotations :: <skip-list>) => ()
   for (lib-annot in annotations)
      let lib = lib-annot.annot-library;
      let deps = dependencies-from-token(lib-annot.annot-token);
      lib-annot.annot-dependencies := deps;

      for (mod-annot in lib-annot.annot-modules)
         let mod = mod-annot.annot-module;
         let deps = dependencies-from-token(mod-annot.annot-token);
         mod-annot.annot-dependencies := deps;
         check-and-flag-module-exports(annotations, lib, mod);
         check-and-flag-module-creates(annotations, lib, mod);
      end for;
      check-and-flag-library-exports(annotations, lib);
   end for;
end method;


define method dependencies-from-token
   (token :: type-union(<library-definer-token>, <module-definer-token>))
=> (dependencies :: <sequence> /* of <use-clause-token> */)
   let clauses = choose(rcurry(instance?, <use-clause-token>), token.namespace-clauses);
   remove-duplicates(clauses, test: same-use-name?)
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
      let lib = lib-annot.annot-library;
      infer-and-import-for-library(annotations, lib);
      order-list-by-dependency(lib-annot.annot-modules);
      for (mod-annot in lib-annot.annot-modules using backward-iteration-protocol)
         let mod = mod-annot.annot-module;
         infer-and-import-for-module(annotations, lib, mod);
         for (bind-annot in mod-annot.annot-bindings)
            let bind = bind-annot.annot-binding;
            infer-and-import-for-binding(annotations, lib, mod, bind);
         end for;
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
of a library or module are updated with any newly-added APIs, and ensures all
definitions are consolidated.
**/
define method make-all-imports-and-defns (annotations :: <skip-list>) => ()
   for (lib-annot in annotations)
      // Import-all-for-library may create new modules.
      // If it does so, we need to redo dependencies so the inner loop is right.
      let lib = lib-annot.annot-library;
      infer-and-import-for-library(annotations, lib);
      order-list-by-dependency(lib-annot.annot-modules);
      for (mod-annot in lib-annot.annot-modules)
         let mod = mod-annot.annot-module;
         infer-and-import-for-module(annotations, lib, mod);
         for (bind-annot in mod-annot.annot-bindings)
            let bind = bind-annot.annot-binding;
            infer-and-import-for-binding(annotations, lib, mod, bind);
         end for;
      end for;
   end for;
end method;


//
// Interchange file data extraction
//


define constant <non-namespace-definition-token> =
      type-union(<class-definer-token>, <constant-definer-token>,
                 <function-definer-token>, <generic-definer-token>,
                 <method-definer-token>, <variable-definer-token>,
                 <domain-definer-token>, <macro-definer-token>);


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


define method same-api-name?
   (token1 :: type-union(<definition-token>, <text-name-token>),
    token2 :: type-union(<definition-token>, <text-name-token>))
=> (same? :: <boolean>)
   case-insensitive-equal?(token1.api-name, token2.api-name)
end method;


define method same-use-name?
   (token1 :: <use-clause-token>, token2 :: <use-clause-token>)
=> (same? :: <boolean>)
   case-insensitive-equal?(token1.use-name, token2.use-name)
end method;


define method same-local-name?
   (item1 :: <named-api-element>, item2 :: <named-api-element>)
=> (same-name? :: <boolean>)
   case-insensitive-equal?(item1.local-name, item2.local-name)
end method;


define method same-source-name? (item1 :: <source-name>, item2 :: <source-name>)
=> (same? :: <boolean>)
   case-insensitive-equal?(item1.source-name, item2.source-name)
end method;


define method has-local-name? (item :: <named-api-element>, name :: <string>)
=> (has-name? :: <boolean>)
   case-insensitive-equal?(item.local-name, name)
end method;


define method has-source-name? (item :: <source-name>, name :: <string>)
=> (has-name? :: <boolean>)
   case-insensitive-equal?(item.source-name, name)
end method;

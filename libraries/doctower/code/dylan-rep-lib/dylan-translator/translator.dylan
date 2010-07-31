module: dylan-translator
synopsis: Contains code in charge of building representations of Dylan APIs.


// TODO: Try to keep the tokens around longer, rather than converting them to
// text, so that I can turn "foo" from "define constant foo = method () ... end"
// into a function. But if I do this, it could interfere with foo if it were
// documented as a constant. Keeping the tokens around could also enable the
// source location of, say, an <empty-binding>, to be the name of the binding
// itself rather than its clause.


/**
Synopsis: Returns representation of Dylan libraries, modules, definitions, etc.

--- Arguments: ---
file-sets - A sequence. Each element of the sequence is a library being
            documented. Specifically, each element is a sequence of the parsed
            files included in that library; a sequence of
            <interchange-file-token>.

--- Values: ---
definitions - A sequence of <definition> objects including all the libraries,
              modules, and bindings.
**/
define method apis-from-dylan (file-sets :: <sequence>)
=> (definitions :: <sequence>)
   verbose-log("Cataloging definitions");
   let context = make(<context>);
   make-api-objects(context, file-sets);
   process-namespace-clauses(context);
   infer-and-merge-definitions(context);
   let definitions = unique-definitions(context);
   
   // Replace simple singleton type fragments with <singleton-type-fragment>.
   visit-type-fragments(definitions, make-singleton-type-fragment);
   
   // Analyze definitions and fill in class information for templates.
   let class-list = class-inheritance-list(definitions);
   inherit-slots(definitions, class-list);
   inherit-init-args(definitions, class-list);
   note-class-functions(definitions, class-list);

   definitions
end method;


define method unique-definitions (context :: <context>) 
=> (definitions :: <sequence>)
   let all-defns = make(<stretchy-vector>);
   let all-bindings = make(<stretchy-vector>);
   for (lib in context.library-definitions)
      all-defns := add-new!(all-defns, lib);
      for (mod in lib.definitions)
         all-defns := add-new!(all-defns, mod);
         all-bindings := union(all-bindings, mod.definitions.element-sequence);
      end for;
   end for;
   concatenate(all-defns, all-bindings)
end method;


/// Synopsis: Create defined libraries, modules, and bindings.
define method make-api-objects (context :: <context>, file-sets :: <sequence>)
=> ()
   make-predefined-apis(context);
   do(curry(make-apis-from-files, context), file-sets);
end method;


define method make-apis-from-files
   (context :: <context>, files :: <sequence>)
=> (library :: <library>)
   
   // Library token
   
   let library-tokens =
         choose-interchange-definitions(<library-definer-token>, files);
   let library = 
         select (library-tokens.size)
            0 =>
               let files = map(source-file, map(token-src-loc, files));
               no-library-in-fileset(filenames: item-string-list(files));
            1 =>
               let token = library-tokens.first;
               make-library-from-definition(context, token);
            otherwise =>
               let dupes = map(token-src-loc, library-tokens);
               multiple-libraries-in-fileset(location: dupes.first,
                     defn-locations: item-string-list(dupes));
         end select;

   // Module tokens
   
   with-context-name (library.canonical-name)
      do(curry(make-module-from-definition, context),
         choose-interchange-definitions(<module-definer-token>, files));
   end with-context-name;
   
   // Module files & definition tokens

   for (file in files)
      let header = file.module-header;
      let file-module = header.hdr-value;
      if (case-insensitive-equal?(file-module, "dylan-user"))

         // Get unscoped documentation comments.
         library.file-markup-tokens := concatenate!
               (library.file-markup-tokens, file.source-record.unscoped-docs);
      else

         // Ensure module exists.
         let module = element(library.definitions, file-module, default: #f);
         unless (module)
            undefined-module-for-interchange-file(location: header.token-src-loc,
                                                  name: header.hdr-value)
         end unless;
         
         let mod-name = make(<module-name>, module: file-module,
                             within: library.canonical-name,
                             source-location: header.token-src-loc);
         with-context-name (mod-name)
            // Create definitions and bindings.
            let defn-tokens = choose-interchange-definitions
                  (<non-namespace-definition-token>, vector(file));
            do(curry(make-bindings-from-definition, context), defn-tokens);
         
            // Get unscoped documentation comments.
            module.file-markup-tokens := concatenate!
                  (module.file-markup-tokens, file.source-record.unscoped-docs);
         end with-context-name;
      end if;
   end for;
   
   library
end method;


define function make-singleton-type-fragment
   (type-frag :: <type-fragment>, #key setter)
=> (slots? :: <boolean>)
   let names = type-frag.fragment-names;
   let (sing-part, expr-part) = partition(curry(\=, $singleton-name), names);
   if (sing-part.size = 1 & expr-part.size = 1)
      let sing-frag = make(<singleton-type-fragment>, text: type-frag.source-text, 
            expression: as(<type-fragment>, expr-part.first),
            source-location: type-frag.source-location);
      setter(sing-frag);
   end if;
   #t
end function;


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

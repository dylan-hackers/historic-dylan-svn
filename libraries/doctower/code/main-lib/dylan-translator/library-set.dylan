module: dylan-translator
synopsis: Working representation of a library and its files.


define class <library-set> (<object>)
   slot libset-library :: false-or(<library>) = #f;
   slot libset-files :: false-or(<sequence>) = #f, init-keyword: #"files";
   slot libset-library-token :: false-or(<library-definer-token>) = #f;
   slot libset-module-tokens :: false-or(<table>) = #f;
   slot libset-definition-tokens :: false-or(<table>) = #f;
end class;


define constant <non-namespace-definition-token> =
      type-union(<class-definer-token>, <constant-definer-token>,
                 <function-definer-token>, <generic-definer-token>,
                 <method-definer-token>, <variable-definer-token>
                 /*, <macro-definer-token>*/);


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
            case-insensitive-equal?(hdr.hdr-keyword, "module")
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


define method make-library-sets (file-sets :: <sequence>)
=> (library-sets :: <table>)
   let libset-table = make(<case-insensitive-string-table>);
   let libsets = map(make-library-set, file-sets);
   for (libset in libsets)
      libset-table[libset.libset-library-token.api-name] := libset
   end for;
   libset-table
end method;


define method make-library-set (files :: <sequence>)
=> (library-set :: <library-set>)
   let library-set = make(<library-set>, files: files);
   
   // Library token
   
   let library-tokens = choose-interchange-definitions(<library-definer-token>, files);
   library-set.libset-library-token :=
         select (library-tokens.size)
            0 =>
               let files = map(source-file, map(token-src-loc, files));
               no-library-in-fileset(filenames: item-string-list(files));
            1 =>
               library-tokens.first;
            otherwise =>
               let dupes = map(token-src-loc, library-tokens);
               multiple-libraries-in-fileset(defn-locations: item-string-list(dupes));
         end select;
   
   // Module tokens
   
   let module-tokens = choose-interchange-definitions(<module-definer-token>, files);
   let module-token-table = make(<case-insensitive-string-table>, size: module-tokens.size);
   library-set.libset-module-tokens := module-token-table;
   do(method (tok :: <module-definer-token>)
         let existing-defn = element(module-token-table, tok.api-name, default: #f);
         when (existing-defn)
            let locations = vector(existing-defn.token-src-loc, tok.token-src-loc);
            duplicate-modules-in-fileset(name: tok.api-name,
                  defn-locations: locations.item-string-list)
         end when;
         module-token-table[tok.api-name] := tok
      end, module-tokens);
   
   // Module files & definitions

   let module-definitions-table = make(<case-insensitive-string-table>);
   library-set.libset-definition-tokens := module-definitions-table;
   let file-headers = map(module-header, files);
   for (file in files, header in file-headers)
      let file-module = header.hdr-value;
      unless (case-insensitive-equal?(file-module, "dylan-user") |
              key-exists?(module-token-table, file-module))
         undefined-module-for-interchange-file(location: header.token-src-loc,
                                               name: header.hdr-value)
      end unless;

      let definitions =
            choose-interchange-definitions(<non-namespace-definition-token>, vector(file));
      let module-definitions =
            element(module-definitions-table, file-module, default: #[]);
      module-definitions := concatenate!(module-definitions, definitions);
      module-definitions-table[file-module] := module-definitions;
   end for;
   
   library-set
end method;

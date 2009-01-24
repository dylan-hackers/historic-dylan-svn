module: dylan-translator

/** Synopsis: Returns APIs in a hierarchal form. **/
define method organize-apis (files :: <sequence> /* of <interchange-file-token> */)
=> (result)
   // Find library.
   let library = extract-library(files);
   
   // Find modules.
   let modules = extract-modules(files, library);
   
   // Sort APIs into module and library.

   #[]
end method;


define method extract-library (files :: <sequence> /* of <interchange-file-token> */)
=> (library :: <library>)
   let libs = choose-interchange-definitions(<library-definer-token>, files);
   select (libs.size)
      0 =>
         let files = map(source-file, map(token-src-loc, files));
         no-library-in-fileset(#f, filenames: item-string-list(files));
      1 =>
         let token = libs.first;
         let lib = make(<library>, source-token: token);
         process-library-definition(lib, token);
         lib;
      otherwise =>
         let dupes = map(token-src-loc, libs);
         multiple-libraries-in-fileset(#f, defn-locations: item-string-list(dupes));
   end select;
end method;


define method extract-modules
   (files :: <sequence> /* of <interchange-file-token> */,
    library :: <library>)
=> (modules :: <sequence> /* of <module> */)
   let mods = choose-interchange-definitions(<module-definer-token>, files);
   select (mods.size)
      0 =>
         let files = map(source-file, map(token-src-loc, files));
         no-modules-in-fileset(#f, filenames: item-string-list(files));
      otherwise =>
         // TODO
         #[];
   end select;
end method;


define method choose-interchange-definitions
   (type :: <class>, ichange-tokens :: <sequence> /* of <interchange-file-token> */)
=> (seq :: <sequence> /* of type */)
   let source-records = choose(true?, map(source-record, ichange-tokens));
   let source-defns = map(definitions, source-records);
   let defns = apply(concatenate, #[], source-defns);
   choose(rcurry(instance?, type), defns);
end method;

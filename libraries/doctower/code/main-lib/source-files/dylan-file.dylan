module: source-files
synopsis: Gets doc markup out of doc files or comment blocks.


define method topics-from-dylan-files (locator-seq :: <sequence>)
=> (topic-seq :: <sequence>)
   let (lid-locators, dylan-locators) = partition(
         method (loc :: <file-locator>) => (bool)
            case-insensitive-equal?("lid", loc.locator-extension)
         end method,
         locator-seq);

   // Collect any .lid files' referenced files into a library set.
   let library-sets = map(dylan-files-from-lid-file, lid-locators);
   
   // Collect remaining .dyl or .dylan files into another library set.
   for (library-set in library-sets)
      for (file in library-set)
         dylan-locators := remove!(dylan-locators, file, test: \=)
      end for;
   end for;

   // Read and parse Dylan files.
   unless (dylan-locators.empty?)
      library-sets := add(library-sets, dylan-locators);
   end unless;

   let parsed-library-sets = map(parse-library-set, library-sets);

   // Generate internal representation of Dylan code.
   let api-definitions = apis-from-dylan(parsed-library-sets);
   
   // Generate topics, sections and placeholders.
   topics-from-dylan(api-definitions)
end method;


define method dylan-files-from-lid-file (lid-locator :: <file-locator>)
=> (file-seq :: <sequence>)
   local method locator-from-filename (filename :: <string>)
         => (file :: <file-locator>)
            let filenames = vector(filename,
                                   concatenate(filename, ".dylan"),
                                   concatenate(filename, ".dyl"));
            block (found-file)
               for (filename in filenames)
                  let locator = merge-locators(as(<file-locator>, filename),
                                               lid-locator);
                  let stream =
                        block ()
                           // if-does-not-exist: #f would be preferred, but that
                           // is not implemented in Gwydion Dylan.
                           make(<file-stream>, locator: locator,
                                if-does-not-exist: #"signal");
                        exception (err :: <file-does-not-exist-error>)
                        end block;
                  if (stream)
                     close(stream);
                     found-file(locator);
                  end if;
               end for;
               file-not-found(location: lid-locator, filename: filename);
            end block;
         end method;

   let interchange = parse-dylan-file(lid-locator);
   let filenames = filenames-from-headers(interchange);
   let locators = map(locator-from-filename, filenames);
   remove-duplicates(locators, test: \=)
end method;


define method filenames-from-headers (token :: <interchange-file-token>)
=> (filenames :: <sequence>)
   local method file-header? (hdr :: <header-token>) => (bool :: <boolean>)
            case-insensitive-equal?(hdr.hdr-keyword, "Files")
         end method;

   let file-headers = choose(file-header?, token.headers);
   when (file-headers.empty?)
      no-header-in-interchange-file(location: token.token-src-loc.source-file,
                                    header: "Files:");
   end when;
   
   let names-per-header = map(rcurry(split, '\n'), map(hdr-value, file-headers));
   let names = apply(concatenate, #[], names-per-header);
   let names = choose(complement(empty?), names); // "Files:" first line may be blank.
   when (names.empty?)
      empty-header-in-interchange-file(location: token.token-src-loc.source-file,
                                       header: "Files:");
   end when;
   names
end method;


define method parse-library-set (locators :: <sequence>)
=> (ichange-files :: <sequence>)
   choose(true?, map(parse-dylan-file, locators));
end method;


define method parse-dylan-file (locator :: <file-locator>)
=> (token :: <interchange-file-token>)
   verbose-log("Parsing %s", locator);
   with-open-file (file = locator)
      let text = make(<canonical-text-stream>, inner-stream: file);
      block ()
         parse-dylan(text, curry(line-col-position, text, at:),
                     file.stream-locator);
      cleanup
         text.close;
      end block;
   end with-open-file
end method;

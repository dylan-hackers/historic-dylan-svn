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

   unless (dylan-locators.empty?)
      library-sets := add(library-sets, dylan-locators);
   end unless;

   let parsed-library-sets = map(parse-library-set, library-sets);
   topics-from-dylan(parsed-library-sets);
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

   let interchange = parse-file(lid-locator);
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
   let names = choose(complement(empty?), names); // Files: first line may be blank.
   when (names.empty?)
      empty-header-in-interchange-file(location: token.token-src-loc.source-file,
                                       header: "Files:");
   end when;
   names
end method;


define method parse-library-set (locators :: <sequence>)
=> (ichange-files :: <sequence>)
   choose(true?, map(parse-file, locators));
end method;


define method parse-file (locator :: <file-locator>)
=> (token :: <interchange-file-token>)
   verbose-log("Parsing %s", locator);
   with-open-file (file = locator)
      let text = make(<canonical-text-stream>, inner-stream: file);
      block ()
         parse-dylan-file(text, file.stream-locator);
      cleanup
         text.close;
      exception (fail :: <parse-failure>)
         let (line, col) = line-col-position(text, at: fail.failure-position);
         let loc = make(<file-source-location>,
                        file: text.inner-stream.inner-stream.stream-locator,
                        start-line: line, start-column: col,
                        end-line: line, end-col: col);
         parse-error-in-dylan(location: loc, expected: fail.parse-expected);
      end block;
   end with-open-file
end method;


/// Synopsis: Records what is known a priori about a block of markup.
/// Discussion: Used to resolve API references and to construct a topic for the
/// block.
define class <topic-context> (<object>)
   /// Synopsis: The default parent topic of all topics in this block.
   ///
   /// Discussion: The default parent for a comment block is a topic in the
   /// reference section of the documentation for the file's library and
   /// module. For a file, there is no parent topic. The result either stands
   /// alone at the top level, or is placed in a hierarchy based on other
   /// directives or the TOC file.
   slot default-parent :: false-or(type-union(<topic>, <section>)) = #f;
   
   /// Synopsis: The skeleton of the first or anonymous topic within the block.
   ///
   /// Discussion: A comment block or file has a main topic. Any topics within
   /// the block after the first are supplementary; they are either sibling
   /// topics to the main one, or subtopics of the main one. The main topic
   /// for an API element can be partially filled out by scanning the source
   /// code; the comment's main topic puts meat on that skeleton.
   ///
   /// The main topic of a comment block or file must be implied, in which case
   /// there will be a skeleton topic, or else must be specified in the comment
   /// block or file itself. In the latter case, this field is #f.
   slot topic :: false-or(<topic>) = #f;
end class;


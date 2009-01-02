module: source-files
synopsis: Gets doc markup out of doc files or comment blocks.


define method topics-from-dylan-files (file-seq :: <sequence>)
=> (topic-seq :: <sequence>)
   let (lid-files, dylan-files) = partition(
         method (file :: <file-stream>) => (bool)
            case-insensitive-equal?("lid", file.stream-locator.locator-extension)
         end method,
         file-seq);

   // Collect any .lid files' referenced files into a library set.
   let library-sets = map(dylan-files-from-lid-file, lid-files);

   // Collect remaining .dyl or .dylan files into another library set.
   for (library-set in library-sets)
      for (file in library-set)
         dylan-files := remove!(dylan-files, file, test: \=)
      end for;
   end for;

   unless (dylan-files.empty?)
      library-sets := add(library-sets, dylan-files);
   end unless;

   // Process them.
   map(topics-from-library-set, library-sets);
end method;


define method dylan-files-from-lid-file (lid-file :: <file-stream>)
=> (file-seq :: <sequence>)
   let lid-locator = lid-file.stream-locator;
   
   local method file-from-filename (filename :: <string>)
         => (file :: false-or(<file-stream>))
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
                     found-file(stream);
                  end if;
               end for;
               file-not-found(lid-locator, filename: filename);
               found-file(#f);
            end block;
         end method;

   let interchange = parse-file(lid-file);
   let filenames = filenames-from-headers(interchange);
   let file-seq = choose(true?, map(file-from-filename, filenames));
   remove-duplicates(file-seq, test: \=)
end method;


define method filenames-from-headers (token :: <interchange-file-token>)
=> (filenames :: <sequence>)
   local method file-header? (hdr :: <header-token>) => (bool :: <boolean>)
            case-insensitive-equal?(hdr.hdr-keyword, "Files")
         end method;

   let file-headers = choose(file-header?, token.headers);
   let headers-names = map(rcurry(split, '\n'), map(hdr-value, file-headers));
   apply(concatenate, #[], headers-names);
end method;


define method topics-from-library-set (files :: <sequence>)
=> (topics :: <sequence>)
   log-object("Library file set",
         map(method (file) as(<string>, file.stream-locator) end, files));

   // Read source code.
   // Generate implicit topics and sections.
   // Associate comments to topics and sections.
   // Read and parse comments.
   // Generate explicit topics, sections, and placeholders.
   #()
end method;


define method parse-file (file :: <file-stream>)
=> (token :: <interchange-file-token>)
   let text = make(<canonical-text-stream>, inner-stream: file);
   block ()
      parse-dylan-file(text, file.stream-locator);
   cleanup
      text.close;
   end block;
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


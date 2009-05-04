module: source-files
synopsis: Calls parser for files containing nothing but markup.

define method topics-from-markup-files (locator-seq :: <sequence>)
=> (topic-seq :: <sequence>)
   let per-file-topics = map(topics-from-markup-file, locator-seq);
   apply(concatenate, #[], per-file-topics)
end method;

define method topics-from-markup-file (locator :: <file-locator>)
=> (topics :: <sequence>)
   // Read and parse file.
   with-open-file (file = locator)
      let text = make(<canonical-text-stream>, inner-stream: file);
      let token =
            block ()
               parse-markup(text, curry(line-col-position, text, at:),
                            file.stream-locator);
            cleanup
               text.close;
            end block;
   
      // Generate explicit topics and placeholders.
      topics-from-markup(token, #f);
   end with-open-file
end method;

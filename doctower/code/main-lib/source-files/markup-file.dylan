module: source-files
synopsis: Calls parser for files containing nothing but markup.

define method topics-from-markup-files (file-seq :: <sequence>)
=> (topic-seq :: <sequence>)
   let per-file-topics = map(topics-from-markup-file, file-seq);
   apply(concatenate, #[], per-file-topics)
end method;

define method topics-from-markup-file (file :: <file-stream>)
=> (topics :: <sequence>)
   // Read and parse file.
   let text = make(<canonical-text-stream>, inner-stream: file);
   let token = block ()
                  parse-markup(text, file.stream-locator);
               cleanup
                  text.close;
               end block;
   
   // Generate explicit topics and placeholders.
   topics-from-markup(token, #f);
end method;

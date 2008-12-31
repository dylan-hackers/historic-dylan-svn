module: source-files
synopsis: Calls parser for files containing nothing but markup.

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

module: workflows


define method create-doc-tree
   (toc-files :: <sequence>, doc-files :: <sequence>, source-files :: <sequence>)
=> (structure :: <ordered-tree>)
   // Collect explicit and implicit topics. Each map returns a list containing
   // lists (one per file) of topics. The apply removes the per-file layer.
   let topics-per-toc-file = map(rcurry(topics-from-file, #"toc-file"), toc-files);
   let topics-per-doc-file = map(rcurry(topics-from-file, #"doc-file"), doc-files);
   let topics-per-source-file = map(rcurry(topics-from-file, #"source-file"), source-files);
   let topics-per-file =
         concatenate(topics-per-toc-file, topics-per-doc-file, topics-per-source-file);
   let topics = apply(concatenate, topics-per-file);
   
   // Check for duplicate IDs and resolve table of contents placeholders.
   log-object("Before res", topics);
   let topics = resolve-topic-placeholders(topics);
   log-object("After res", topics);
   
   // Check for ambiguous titles and arrange explicit and implicit topics.
   let doc-tree = arrange-topics(topics);

   // Merge and replace implicit topics.
   // Finalize table of contents.
   doc-tree
end method;


define method topics-from-file (file :: <file-stream>, type == #"doc-file")
=> (topics :: <sequence>)
   // Read and parse file.
   let tokens = parse-markup(file);
   
   // Generate explicit topics and placeholders.
   process-markup(tokens, #f);
end method;

define method topics-from-file (file :: <file-stream>, type == #"source-file")
=> (topics :: <sequence>)
   // Read source code.
   // Generate implicit topics and sections.
   // Associate comments to topics and sections.
   // Read and parse comments.
   // Generate explicit topics, sections, and placeholders.
   #()
end method;

define method topics-from-file (file :: <file-stream>, type == #"toc-file")
=> (topics :: <sequence>)
   // Read and parse table of contents.
   // Generate implicit topics and placeholders.
   #()
end method;

module: workflows


define method create-doc-tree
   (toc-files :: <sequence>, doc-files :: <sequence>, source-files :: <sequence>)
=> (structure :: <ordered-tree>)
   // Collect tables of content.
   let tocs = map(toc-from-file, toc-files);
   // log-object("Table of contents", tocs);

   // Collect explicit and implicit topics. Each map returns a list containing
   // lists (one per file) of topics. The apply removes the per-file layer.
   let topics-per-doc-file = map(topics-from-markup-file, doc-files);
   let topics-per-source-file = map(topics-from-dylan-file, source-files);
   let topics-per-file = concatenate(topics-per-doc-file, topics-per-source-file);
   let topics = apply(concatenate, topics-per-file);
   
   // Combine API topic fragments.
   // TODO: Combine topics that have same API name, type, library, and module.
   
   // Check for duplicate IDs and resolve table of contents placeholders.

   // log-object("Before res", topics);
   let (topics, ambig-topic-table, tocs) =
      resolve-topic-placeholders(topics, tocs);
   // log-object("After res", topics);
   
   // Check for ambiguous titles and arrange explicit and implicit topics.

   let handler <need-locations> =
         method (cond, next-handler)
            let topics = element(ambig-topic-table, cond.specifier-for-locations,
                                 default: #());
            map(source-location, topics)
         end;

   let doc-tree = arrange-topics(topics, tocs);

   // Create structural topics and arrange API lists.
   // Finalize table of contents.
   doc-tree
end method;

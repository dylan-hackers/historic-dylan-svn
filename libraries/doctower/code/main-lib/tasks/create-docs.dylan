module: tasks


define method create-doc-tree
   (toc-files :: <sequence>, doc-files :: <sequence>, source-files :: <sequence>)
=> (structure :: <ordered-tree>)
   // Collect tables of content.
   let tocs = map(toc-from-file, toc-files);
   // log-object("Table of contents", tocs);

   // Collect authored and generated topics.
   let doc-file-topics = topics-from-markup-files(doc-files);
   let src-file-topics = topics-from-dylan-files(source-files);
   let topics = concatenate(doc-file-topics, src-file-topics);
   let grouped-topics = group-mergeable-topics(topics);
   
   // Check and combine API topic fragments.
   let topics = map(check-and-merge-topics, grouped-topics);
   
   // Check for duplicate IDs and resolve table of contents placeholders.

   // log-object("Before res", topics);
   let (topics, ambig-topic-table, tocs) =
      resolve-topic-placeholders(topics, tocs);
   // log-object("After res", topics);
   
   // Check for ambiguous titles and arrange authored and generated topics.

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

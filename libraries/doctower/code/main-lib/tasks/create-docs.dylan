module: tasks


define method create-doc-tree
   (toc-files :: <sequence>, doc-files :: <sequence>, source-files :: <sequence>)
=> (structure :: <ordered-tree>)
   // Create templates.
   create-topic-templates();

   // Collect tables of content.
   let tocs = map(toc-from-file, toc-files);

   // Collect authored and generated topics.
   let doc-file-topics = topics-from-markup-files(doc-files);
   let (src-file-topics, catalog-topics) = topics-from-dylan-files(source-files);
   let topics = concatenate(doc-file-topics, src-file-topics);
   
   verbose-log("Organizing documentation");
   
   // Check and combine API topic fragments.
   let grouped-topics = group-mergeable-topics(topics);
   let topics = map(check-and-merge-topics, grouped-topics);
   
   // Check for duplicate IDs and resolve table of contents and placeholders.
   let (topics, tocs) = resolve-topic-placeholders(topics, tocs, catalog-topics);
   
   // Discard templates; all topics will have been created.
   discard-topic-templates();
   
   // Check for ambiguous titles and arrange authored and generated topics.
   let doc-tree = arrange-topics(topics, tocs);

   // Create API lists.
   // Finalize table of contents.
   doc-tree
end method;

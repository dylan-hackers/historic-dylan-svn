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
   
   // Discard templates; all topics will have been created.
   discard-topic-templates();
   
   verbose-log("Organizing documentation");
   
   // Check and combine API topic fragments.
   let grouped-topics = group-mergeable-topics(topics);
   let topics = map(check-and-merge-topics, grouped-topics);
   
   // Index topics by various means.
   let topic-id-table = topics-by-id(topics);
   let topic-fqn-table = topics-by-fqn(topics);
   let (topic-title-table, topic-dup-title-table) = topics-by-title(topics);
   
   // Resolve table of contents and topic and local placeholders.
   // Check for duplicate IDs. Discard unused catalog topics.
   check-topic-ids(topics, id-table: topic-id-table, title-table: topic-title-table);
   let topics = resolve-xref-placeholders(topics, id-table: topic-id-table,
         fqn-table: topic-fqn-table, title-table: topic-title-table);
   let (topics, tocs) = resolve-topic-placeholders(topics, tocs, catalog-topics,
         id-table: topic-id-table, fqn-table: topic-fqn-table,
         title-table: topic-title-table, dup-title-table: topic-dup-title-table);
   
   // Done with indexes.
   topic-id-table := topic-fqn-table := topic-title-table := topic-dup-title-table
         := #f;
   
   // Check for ambiguous titles and arrange authored and generated topics.
   let doc-tree = arrange-topics(topics, tocs);
   
   // Ensure every topic has an ID.
   ensure-topic-ids(doc-tree);

   // Create API lists.
   replace-content-placeholders(doc-tree);
   
   // Finalize table of contents.
   
   doc-tree
end method;

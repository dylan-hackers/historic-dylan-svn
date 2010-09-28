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
   
   // Index topics and sections by various means.
   // Check for duplicate IDs and other issues.
   let (target-res, dup-titles) = topics.resolution-info;
   
   // Resolve table of contents and topic and local placeholders.
   // Discard unused catalog topics.
   let (topics, tocs) = resolve-target-placeholders(topics, tocs,
         catalog-topics, target-res, dup-titles);
   
   // Check for ambiguous titles and arrange authored and generated topics.
   let doc-tree = arrange-topics(topics, tocs);
   
   // Ensure every topic has an ID.
   ensure-topic-ids(doc-tree);

   // Create API lists.
   replace-content-placeholders(doc-tree);
   
   // Finalize table of contents.
   // TODO: I think this means creating the general index and other structural pages.
   
   doc-tree
end method;


define method create-output-files (structure :: <ordered-tree>) => ()
   verbose-log("Designing navigation");
   let (output-files, target-files) = chunk-topics(structure);
   
   verbose-log("Generating documentation files");
   // for (file-info in output-files)
   //    write-output-file(file-info, links, link-targets)
   // end for;
end method;

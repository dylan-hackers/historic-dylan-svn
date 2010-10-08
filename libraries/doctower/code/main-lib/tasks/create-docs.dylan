module: tasks


define method create-doc-tree
   (toc-files :: <sequence>, doc-files :: <sequence>, source-files :: <sequence>)
=> (structure :: <ordered-tree>)
   // Delete existing output.
   if (*output-directory*.file-exists?)
      verbose-log("Deleting %s", *output-directory*);
      with-file-error-handlers (default-locator: *output-directory*)
         delete-directory(*output-directory*);
      end with-file-error-handlers
   end if;
   
   // Create templates.
   verbose-log("Reading topic templates");
   create-templates($topic-templates);

   // Collect tables of content.
   let tocs = map(toc-from-file, toc-files);

   // Collect authored and generated topics.
   let doc-file-topics = topics-from-markup-files(doc-files);
   let (src-file-topics, catalog-topics) = topics-from-dylan-files(source-files);
   let topics = concatenate(doc-file-topics, src-file-topics);
   
   // Discard templates; all topics will have been created.
   discard-templates();
   
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
   
   // Create API lists.
   replace-content-placeholders(doc-tree);
   
   // Finalize table of contents.
   // TODO: I think this means creating the general index and other structural pages.
   
   doc-tree
end method;


define method create-output-files (doc-tree :: <ordered-tree>) => ()
   verbose-log("Designing navigation");
   let link-map = doc-tree.topic-link-map;
   let target-ids = doc-tree.target-navigation-ids;
   
   for (output-type in *output-types*)
      let output-type-name = as(<string>, output-type).as-uppercase;
      
      verbose-log("Reading %s templates", output-type-name);
      create-templates(output-type.output-templates, sanitizer: xml-sanitizer);
      
      verbose-log("Generating %s documentation files", output-type-name);
      let (topic-output-files, special-output-files, output-files)
            = output-file-info(output-type, doc-tree);
      let link-targets
            = target-link-info(output-type, doc-tree, target-ids, topic-output-files);

      for (file-info in output-files)
         write-output-file(output-type, file-info, link-map, link-targets,
               special-output-files)
      end for;
      
      discard-templates();
   end for;
end method;

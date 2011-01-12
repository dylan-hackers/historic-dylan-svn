module: dylan-topics
synopsis: Code to generate library and module topics.


define method make-source-topics (defn :: <library>) 
=> (topics :: <sequence>, catalog-topics :: <sequence>)
   let fqn = defn.canonical-qualified-name;

   // Create generated topic.

   let generated-topic = make(<library-doc>, source-location: defn.source-location,
         id: defn.canonical-id, title: defn.canonical-title, generated: #t,
         qualified-name: fqn, scope: fqn, existent-api: #t,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);

   make-namespace-names(generated-topic, defn);

   // Create body of generated topics.
   
   let vars = table(<case-insensitive-string-table>, "lib" => defn);
   let topics = topics-from-template(#"library-topic", generated-topic, vars);
   let catalog-topics = topics-from-template(#"lib-catalog-topics", #f, vars);
   let topics = concatenate!(topics, catalog-topics);

   // Create authored topics.
   
   unless (defn.markup-tokens.empty?)
      let context-topic = make(<library-doc>, source-location: defn.source-location,
            id: defn.canonical-id, title: defn.canonical-title, generated: #f,
            qualified-name: fqn, scope: fqn, existent-api: #t,
            title-id-source-location: $generated-source-location,
            qualified-name-source-location: $generated-source-location);
      let authored-topics = make-authored-topics(defn.markup-tokens, context-topic);
      topics := concatenate!(topics, authored-topics);
   end unless;
   
   // Create file markup topics.
   
   when (instance?(defn, <defined-namespace>))
      let file-topics = make-authored-topics(defn.file-markup-tokens, #f);
      topics := concatenate!(topics, file-topics);
   end when;
   
   // Set catalog topic scopes.
   do(curry(qualified-scope-name-setter, fqn), catalog-topics);

   values(topics, catalog-topics)
end method;


define method make-source-topics (defn :: <module>) 
=> (topics :: <sequence>, catalog-topics :: <sequence>)
   let fqn = defn.canonical-qualified-name;
   let namespace = fqn.enclosing-qualified-name;

   // Create generated topic.

   let generated-topic = make(<module-doc>, source-location: defn.source-location,
         id: defn.canonical-id, title: defn.canonical-title, generated: #t,
         qualified-name: fqn, scope: fqn, namespace: namespace, existent-api: #t,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);

   make-namespace-names(generated-topic, defn);

   // Create body of generated topic.
   
   let vars = table(<case-insensitive-string-table>, "mod" => defn);
   let topics = topics-from-template(#"module-topic", generated-topic, vars);
   let catalog-topics = topics-from-template(#"mod-catalog-topics", #f, vars);
   let topics = concatenate!(topics, catalog-topics);

   // Create authored topics.
   
   unless (defn.markup-tokens.empty?)
      let context-topic = make(<module-doc>, source-location: defn.source-location,
            id: defn.canonical-id, title: defn.canonical-title, generated: #f,
            qualified-name: fqn, scope: fqn, namespace: namespace, existent-api: #t,
            title-id-source-location: $generated-source-location,
            qualified-name-source-location: $generated-source-location);
      let authored-topics = make-authored-topics(defn.markup-tokens, context-topic);
      topics := concatenate!(topics, authored-topics);
   end unless;
   
   // Create file markup topics.
   
   when (instance?(defn, <defined-namespace>))
      let file-topics = make-authored-topics(defn.file-markup-tokens, #f);
      topics := concatenate!(topics, file-topics);
   end when;

   // Set catalog topic scopes.
   do(curry(qualified-scope-name-setter, fqn), catalog-topics);

   values(topics, catalog-topics)
end method;

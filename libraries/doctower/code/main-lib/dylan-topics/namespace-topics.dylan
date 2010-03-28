module: dylan-topics


define method make-source-topics (defn :: <library>) 
=> (topics :: <sequence>)

   // Create generated topic.

   let generated-topic = make(<library-doc>, source-location: defn.source-location,
         id: defn.canonical-id, title: defn.canonical-title, generated: #t,
         qualified-name: defn.definition-qualified-name, existent-api: #t,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);

   // Create body of generated topic.
   
   let template = topic-template(#"library-topic");
   let vars = table(<case-insensitive-string-table>, "lib" => defn);
   let topics = topics-from-template(template, generated-topic, vars);

   // Create authored topics.
   
   unless (defn.markup-tokens.empty?)
      let context-topic = make(<library-doc>, source-location: defn.source-location,
            id: defn.canonical-id, title: defn.canonical-title, generated: #f,
            qualified-name: defn.definition-qualified-name, existent-api: #t,
            title-id-source-location: $generated-source-location,
            qualified-name-source-location: $generated-source-location);
      let authored-topics = make-authored-topics(defn.markup-tokens, context-topic);
      topics := concatenate!(topics, authored-topics);
   end unless;
   
   // Create file markup topics.
   
   when (instance?(defn, <defined-namespace>))
      // TODO: Am I going to need some sort of lookup context for these topics?
      let file-topics = make-authored-topics(defn.file-markup-tokens, #f);
      topics := concatenate!(topics, file-topics);
   end when;

   topics
end method;


define method make-source-topics (defn :: <module>) 
=> (topics :: <sequence>)

   // Create generated topic.

   let generated-topic = make(<module-doc>, source-location: defn.source-location,
         id: defn.canonical-id, title: defn.canonical-title, generated: #t,
         qualified-name: defn.definition-qualified-name, existent-api: #t,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);

   // Create body of generated topic.
   
   let template = topic-template(#"module-topic");
   let vars = table(<case-insensitive-string-table>, "mod" => defn);
   let topics = topics-from-template(template, generated-topic, vars);

   // Create authored topics.
   
   unless (defn.markup-tokens.empty?)
      let context-topic = make(<module-doc>, source-location: defn.source-location,
            id: defn.canonical-id, title: defn.canonical-title, generated: #f,
            qualified-name: defn.definition-qualified-name, existent-api: #t,
            title-id-source-location: $generated-source-location,
            qualified-name-source-location: $generated-source-location);
      let authored-topics = make-authored-topics(defn.markup-tokens, context-topic);
      topics := concatenate!(topics, authored-topics);
   end unless;
   
   // Create file markup topics.
   
   when (instance?(defn, <defined-namespace>))
      // TODO: Am I going to need some sort of lookup context for these topics?
      let file-topics = make-authored-topics(defn.file-markup-tokens, #f);
      topics := concatenate!(topics, file-topics);
   end when;

   topics
end method;

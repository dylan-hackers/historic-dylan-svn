module: dylan-topics


define method make-source-topics (defn :: <library>) 
=> (topics :: <sequence>)

   // Create implicit topic.

   let implicit-topic = make(<library-doc>, source-location: defn.source-location,
         id: defn.canonical-id, title: defn.canonical-title, implicit: #t,
         title-id-source-location: $generated-source-location,
         qualified-name: defn.definition-qualified-name);

   // Create body of implicit topic.
   
   let template = topic-template(#"library-topic");
   let vars = table(<case-insensitive-string-table>, "lib" => defn);
   let topics = topics-from-template(template, implicit-topic, vars);

   // Create explicit topics.
   
   unless (defn.markup-tokens.empty?)
      let context-topic = make(<library-doc>, source-location: defn.source-location,
            id: defn.canonical-id, title: defn.canonical-title, implicit: #f,
            title-id-source-location: $generated-source-location,
            qualified-name: defn.definition-qualified-name);
      let explicit-topics = make-explicit-topics(defn.markup-tokens, context-topic);
      topics := concatenate!(topics, explicit-topics);
   end unless;
   
   // Create file markup topics.
   
   when (instance?(defn, <defined-namespace>))
      // TODO: Am I going to need some sort of lookup context for these topics?
      let file-topics = make-explicit-topics(defn.file-markup-tokens, #f);
      topics := concatenate!(topics, file-topics);
   end when;

   topics
end method;


define method make-source-topics (defn :: <module>) 
=> (topics :: <sequence>)

   // Create implicit topic.

   let implicit-topic = make(<module-doc>, source-location: defn.source-location,
         id: defn.canonical-id, title: defn.canonical-title, implicit: #t,
         title-id-source-location: $generated-source-location,
         qualified-name: defn.definition-qualified-name);

   // Create body of implicit topic.
   
   let template = topic-template(#"module-topic");
   let vars = table(<case-insensitive-string-table>, "mod" => defn);
   let topics = topics-from-template(template, implicit-topic, vars);

   // Create explicit topics.
   
   unless (defn.markup-tokens.empty?)
      let context-topic = make(<module-doc>, source-location: defn.source-location,
            id: defn.canonical-id, title: defn.canonical-title, implicit: #f,
            title-id-source-location: $generated-source-location,
            qualified-name: defn.definition-qualified-name);
      let explicit-topics = make-explicit-topics(defn.markup-tokens, context-topic);
      topics := concatenate!(topics, explicit-topics);
   end unless;
   
   // Create file markup topics.
   
   when (instance?(defn, <defined-namespace>))
      // TODO: Am I going to need some sort of lookup context for these topics?
      let file-topics = make-explicit-topics(defn.file-markup-tokens, #f);
      topics := concatenate!(topics, file-topics);
   end when;

   topics
end method;

module: dylan-topics


// TODO: Replace programmatic section construction with template system.


define method make-source-topics (defn :: <library>) 
=> (topics :: <sequence>)
   let implicit-topic = make(<library-doc>, source-location: defn.source-location,
         id: defn.canonical-id, title: defn.canonical-title, implicit: #t,
         title-id-source-location: $generated-source-location,
         qualified-name: defn.definition-qualified-name);

   let topics = vector(implicit-topic);
   
   unless (defn.markup-tokens.empty?)
      let context-topic = make(<library-doc>, source-location: defn.source-location,
            id: defn.canonical-id, title: defn.canonical-title, implicit: #f,
            title-id-source-location: $generated-source-location,
            qualified-name: defn.definition-qualified-name);
      let explicit-topics = make-explicit-topics(defn.markup-tokens, context-topic);
      topics := concatenate!(topics, explicit-topics);
   end unless;
   
   // Create content and sections.
   
   let para = formatted-paragraph("The %s library.",
         defn.canonical-name.local-name.as-titlecase);
   implicit-topic.content := topic-content-seq(para);

   implicit-topic.definitions-section := make-definitions-section(defn);

   let modules-content = content-seq(list-of-directive(#"modules"));
   when (instance?(defn, <defined-library>)
         & ~defn.unknown-reexport-sources.empty?)
      modules-content := add!(modules-content,
            formatted-paragraph("May export other modules from these libraries:"));
   end when;
   
   implicit-topic.modules-section := 
         make(<section>, source-location: $generated-source-location,
              title: title-seq("Modules"), content: modules-content);
   
   topics
end method;


define method make-source-topics (defn :: <module>) 
=> (topics :: <sequence>)
   let implicit-topic = make(<module-doc>, source-location: defn.source-location,
         id: defn.canonical-id, title: defn.canonical-title, implicit: #t,
         title-id-source-location: $generated-source-location,
         qualified-name: defn.definition-qualified-name);
   let topics = vector(implicit-topic);
   
   unless (defn.markup-tokens.empty?)
      let context-topic = make(<module-doc>, source-location: defn.source-location,
            id: defn.canonical-id, title: defn.canonical-title, implicit: #f,
            title-id-source-location: $generated-source-location,
            qualified-name: defn.definition-qualified-name);
      let explicit-topics = make-explicit-topics(defn.markup-tokens, context-topic);
      topics := concatenate!(topics, explicit-topics);
   end unless;
   
   // Create content and sections.
   
   let para = formatted-paragraph("The %s module.",
         defn.canonical-name.local-name.as-titlecase);
   implicit-topic.content := topic-content-seq(para);

   implicit-topic.definitions-section := make-definitions-section(defn);
   
   // TODO: Classes, etc., sections.

   topics
end method;


define method make-definitions-section (defn :: <library>)
=> (section :: false-or(<section>))
   when (instance?(defn.source-location, <file-source-location>))
      let location = defn.source-location;
      let loc-para = location.location-paragraph;
      make(<section>, source-location: $generated-source-location,
           title: title-seq("Definition"), content: content-seq(loc-para))
   end when
end method;


define method make-definitions-section (defn :: <module>)
=> (section :: false-or(<section>))
   // TODO: This.
end method;
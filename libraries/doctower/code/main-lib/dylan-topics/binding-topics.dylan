module: dylan-topics


// TODO: Replace programmatic section construction with template system.


define method make-source-topics (defn :: <binding>) 
=> (topics :: <sequence>)
   let generated-topic = make(<binding-doc>, source-location: defn.source-location,
         id: defn.canonical-id, title: defn.canonical-title, generated: #t,
         existent-api: #t, qualified-name: defn.definition-qualified-name,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);
   vector(generated-topic)
end method;


define method make-source-topics (defn :: <class-binding>) 
=> (topics :: <sequence>)
   let generated-topic = make(<class-doc>, source-location: defn.source-location,
         id: defn.canonical-id, title: defn.canonical-title, generated: #t,
         existent-api: #t, qualified-name: defn.definition-qualified-name,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);
   vector(generated-topic)
end method;


define method make-source-topics (defn :: <function-binding>) 
=> (topics :: <sequence>)
   let generated-topic = make(<function-doc>, source-location: defn.source-location,
         id: defn.canonical-id, title: defn.canonical-title, generated: #t,
         existent-api: #t, qualified-name: defn.definition-qualified-name,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);
   vector(generated-topic)
end method;


define method make-source-topics
   (defn :: type-union(<constant-binding>, <variable-binding>))
=> (topics :: <sequence>)
   let generated-topic = make(<variable-doc>, source-location: defn.source-location,
         id: defn.canonical-id, title: defn.canonical-title, generated: #t,
         existent-api: #t, qualified-name: defn.definition-qualified-name,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);
   vector(generated-topic)
end method;


define method make-source-topics (defn :: <macro-binding>) 
=> (topics :: <sequence>)
   let generated-topic = make(<macro-doc>, source-location: defn.source-location,
         id: defn.canonical-id, title: defn.canonical-title, generated: #t,
         existent-api: #t, qualified-name: defn.definition-qualified-name,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);
   vector(generated-topic)
end method;


define method make-source-topics (defn :: <generic-binding>) 
=> (topics :: <sequence>)
   let generic-topic = make(<generic-doc>, source-location: defn.source-location,
         id: defn.canonical-id, title: defn.canonical-title, generated: #t,
         existent-api: #t, qualified-name: defn.definition-qualified-name,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);
   let topics = vector(generic-topic);
   
   for (method-defn :: <implicit-generic-defn> in defn.implicit-defns)
      let method-params = method-defn.param-list.req-params;
      let topic-id = canonical-id(defn, method-params: method-params);
      let topic-title = canonical-title(defn, method-params: method-params);
      let qual-name = definition-qualified-name(defn, method-params: method-params);
      let method-topic = make(<function-doc>, generated: #t, existent-api: #t,
            source-location: method-defn.source-location,
            qualified-name: qual-name, id: topic-id, title: topic-title,
            title-id-source-location: $generated-source-location,
            qualified-name-source-location: $generated-source-location);
      topics := add!(topics, method-topic);
   end for;
   topics
end method;


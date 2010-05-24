module: dylan-topics


define method make-source-topics (binding :: <class-binding>) 
=> (topics :: <sequence>, catalog-topics :: <sequence>)
   let generated-topic = make(<class-doc>, source-location: binding.source-location,
         id: binding.canonical-id, title: binding.canonical-title, generated: #t,
         existent-api: #t, qualified-name: binding.definition-qualified-name,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);
   
   // Create body of generated topic.
   
   let vars = table(<case-insensitive-string-table>, "class" => binding);
   let topics = topics-from-template
         (topic-template(#"class-topic"), generated-topic, vars);

   // Create authored topics.
   
   unless (binding.markup-tokens.empty?)
      let context-topic = make(<class-doc>, source-location: binding.source-location,
            id: binding.canonical-id, title: binding.canonical-title, generated: #f,
            qualified-name: binding.definition-qualified-name, existent-api: #t,
            title-id-source-location: $generated-source-location,
            qualified-name-source-location: $generated-source-location);
      let authored-topics = make-authored-topics(binding.markup-tokens, context-topic);
      topics := concatenate!(topics, authored-topics);
   end unless;
   
   values(topics, #[])
end method;


//
// Template keyword processing
//


define method template-type (init-arg :: <init-arg>)
=> (type :: false-or(<type-fragment>))
   init-arg.type
end method;


define method template-code? (type :: false-or(<type-fragment>))
=> (code? :: <boolean>)
   type & ~type.simple-name?
end method;


define method template-link? (type :: false-or(<type-fragment>))
=> (link? :: <boolean>)
   type & type.simple-name?
end method;


define method template-default (init-arg :: <init-arg>)
=> (default :: false-or(<code-fragment>))
   init-arg.init-spec
end method;


//
// Template applicable and returning methods
//


define method template-funcs-on-class (defn :: <class-binding>)
=> (funcs :: <sequence>)
   // TODO: template-funcs-on-class
   #[]
end method;


define method template-funcs-returning-class (defn :: <class-binding>)
=> (funcs :: <sequence>)
   // TODO: template-funcs-returning-class
   #[]
end method;


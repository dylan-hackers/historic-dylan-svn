module: dylan-topics
synopsis: Code to generate constant, empty binding, and macro docs.


/// Empty bindings cannot have user-authored documentation; they must be
/// documented as a class, etc. But if they aren't, this generates a page for
/// them.
define method make-source-topics (binding :: <empty-binding>) 
=> (topics :: <sequence>, catalog-topics :: <sequence>)
   let fqn = binding.canonical-qualified-name;
   let namespace = fqn.enclosing-qualified-name;
   let generated-topic = make(<unbound-doc>, generated: #t, existent-api: #t,
         id: binding.canonical-id, title: binding.canonical-title,
         qualified-name: fqn, namespace: namespace,
         source-location: binding.source-location,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);

   make-namespace-names(generated-topic, binding);
   let vars = table(<case-insensitive-string-table>, "unbound" => binding);
   let topics = topics-from-template(#"unbound-topic", generated-topic, vars);

   values(topics, #[]);
end method;


define method make-source-topics (binding :: <placeholder-binding>)
=> (topics :: <sequence>, catalog-topics :: <sequence>)
   let fqn = binding.canonical-qualified-name;
   let namespace = fqn.enclosing-qualified-name;
   let generated-topic = make(<placeholder-doc>, generated: #t, existent-api: #t,
         id: binding.canonical-id, title: binding.canonical-title,
         qualified-name: fqn, namespace: namespace,
         source-location: binding.source-location,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);
   
   make-namespace-names(generated-topic, binding);
   let vars = table(<case-insensitive-string-table>, "placeholder" => binding);
   let topics = topics-from-template(#"placeholder-topic", generated-topic, vars);
   values(topics, #[])
end method;


define method make-source-topics (binding :: <constant-binding>)
=> (topics :: <sequence>, catalog-topics :: <sequence>)
   make-const/var-topics(binding, "const", #"constant", #"constant-topic")
end method;

define method make-source-topics (binding :: <variable-binding>)
=> (topics :: <sequence>, catalog-topics :: <sequence>)
   make-const/var-topics(binding, "var", #"variable", #"variable-topic")
end method;


define method make-const/var-topics
   (binding :: type-union(<constant-binding>, <variable-binding>),
    template-var :: <string>, topic-type :: <symbol>, template-name :: <symbol>)
=> (topics :: <sequence>, catalog-topics :: <sequence>)
   let fqn = binding.canonical-qualified-name;
   let namespace = fqn.enclosing-qualified-name;
   
   // Create body of generated topic.
   
   let generated-topic = make(<variable-doc>,
         generated: #t, existent-api: #t, topic-type: topic-type,
         id: binding.canonical-id, title: binding.canonical-title,
         qualified-name: fqn, namespace: namespace,
         source-location: binding.source-location,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);

   make-namespace-names(generated-topic, binding);
   let vars = table(<case-insensitive-string-table>, template-var => binding);
   let topics = topics-from-template(template-name, generated-topic, vars);

   // Create authored topics.
   
   let authored-topic = make(<variable-doc>,
         generated: #f, existent-api: #t, topic-type: topic-type,
         id: binding.canonical-id, title: binding.canonical-title,
         qualified-name: fqn, namespace: namespace,
         source-location: binding.source-location,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);

   let authored-topics = make-authored-topics(binding.markup-tokens, authored-topic);
   topics := concatenate!(topics, authored-topics);

   values(topics, #[])
end method;


define method make-source-topics (binding :: <macro-binding>) 
=> (topics :: <sequence>, catalog-topics :: <sequence>)
   let fqn = binding.canonical-qualified-name;
   let namespace = fqn.enclosing-qualified-name;
   
   // Create body of generated topic.
   
   let generated-topic = make(<macro-doc>, generated: #t, existent-api: #t,
         id: binding.canonical-id, title: binding.canonical-title,
         qualified-name: fqn, namespace: namespace,
         source-location: binding.source-location,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);

   make-namespace-names(generated-topic, binding);
   let vars = table(<case-insensitive-string-table>, "macro" => binding);
   let topics = topics-from-template(#"macro-topic", generated-topic, vars);

   // Create authored topics.
   
   let authored-topic = make(<macro-doc>, generated: #f, existent-api: #t,
         id: binding.canonical-id, title: binding.canonical-title,
         qualified-name: fqn, namespace: namespace,
         source-location: binding.source-location,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);

   let authored-topics = make-authored-topics(binding.markup-tokens, authored-topic);
   topics := concatenate!(topics, authored-topics);
   
   values(topics, #[]);
end method;


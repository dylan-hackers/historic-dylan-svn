module: dylan-topics
synopsis: Code to generate class topics.


define method make-source-topics (binding :: <class-binding>) 
=> (topics :: <sequence>, catalog-topics :: <sequence>)
   
   // Create body of generated topic.
   
   let generated-topic = make(<class-doc>, generated: #t, existent-api: #t,
         id: binding.canonical-id, title: binding.canonical-title,
         qualified-name: binding.definition-qualified-name,
         source-location: binding.source-location,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);

   let vars = table(<case-insensitive-string-table>, "class" => binding);
   let topics = topics-from-template(#"class-topic", generated-topic, vars);
         
   // Create authored topics.
   
   let authored-topic = make(<class-doc>, generated: #f, existent-api: #t,
         id: binding.canonical-id, title: binding.canonical-title,
         qualified-name: binding.definition-qualified-name,
         source-location: binding.source-location,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);

   let authored-topics = make-authored-topics(binding.markup-tokens, authored-topic);
   topics := concatenate!(topics, authored-topics);
   
   // Replace generated init-arg docs with individual init-arg markup.

   if (binding.explicit-defn)
      let init-arg-items = binding.explicit-defn.init-args;
      let item-defn-list = generated-topic.keywords-section.defn-list;
      if (item-defn-list)
         replace-list-item-docs(item-defn-list, init-arg-items)
      end if;
      
      // If there is init-arg documentation from individual init-args AND from
      // the overall manually-authored class documentation, use the overall
      // class documentation. That is more likely to be focused, since
      // individual init-arg docs may only be intended for getters and setters.
      if (~init-arg-items.empty? & authored-topic.keywords-section)
         let tokens = init-arg-items.all-markup-tokens;
         unused-docs-in-topic(location: authored-topic.keywords-section.source-location,
               doc-locations: map(token-src-loc, tokens).item-string-list);
      end if;
   end if;
   
   values(topics, #[])
end method;


define method defn-list-item-label (init-arg :: <init-arg>) => (name :: <string>)
   format-to-string("%s:", init-arg.symbol)
end method;

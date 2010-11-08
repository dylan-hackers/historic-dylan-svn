module: dylan-topics


define method make-source-topics (binding :: <function-binding>)
=> (topics :: <sequence>, catalog-topics :: <sequence>)
   let fqn = binding.definition-qualified-name;
   let namespace = fqn.enclosing-qualified-name;

   // Create body of generated topic.
   
   let generated-topic = make(<function-doc>,
         generated: #t, existent-api: #t, topic-type: #"function",
         id: binding.canonical-id, title: binding.canonical-title,
         qualified-name: fqn, namespace: namespace,
         source-location: binding.source-location,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);

   make-alias-titles(generated-topic, binding);
   let vars = table(<case-insensitive-string-table>, "func" => binding);
   let topics = topics-from-template(#"function-topic", generated-topic, vars);

   // Create authored topics.
   
   let authored-topic = make(<function-doc>,
         generated: #f, existent-api: #t, topic-type: #"function",
         id: binding.canonical-id, title: binding.canonical-title,
         qualified-name: fqn, namespace: namespace,
         source-location: binding.source-location,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);

   let authored-topics = make-authored-topics(binding.markup-tokens, authored-topic);
   topics := concatenate!(topics, authored-topics);
   
   // Add argument and value documentation from argument- or value-specific markup.

   if (binding.explicit-defn)
      document-args/vals(binding.explicit-defn, generated-topic, authored-topic)
   end if;

   values(topics, #[])
end method;


define method make-source-topics (binding :: <generic-binding>)
=> (topics :: <sequence>, catalog-topics :: <sequence>)
   let fqn = binding.definition-qualified-name;
   let namespace = fqn.enclosing-qualified-name;

   // Create body of generated topic.
   
   let generated-topic = make(<generic-doc>, generated: #t, existent-api: #t,
         id: binding.canonical-id, title: binding.canonical-title,
         qualified-name: fqn, namespace: namespace,
         source-location: binding.source-location,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);

   make-alias-titles(generated-topic, binding);
   let vars = table(<case-insensitive-string-table>, "gen" => binding);
   let topics = topics-from-template(#"generic-topic", generated-topic, vars);

   // Create authored topics.
   
   let authored-topic = make(<generic-doc>, generated: #f, existent-api: #t,
         id: binding.canonical-id, title: binding.canonical-title,
         qualified-name: fqn, namespace: namespace,
         source-location: binding.source-location,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);

   let authored-topics = make-authored-topics(binding.markup-tokens, authored-topic);
   topics := concatenate!(topics, authored-topics);
   
   // Add argument and value documentation from argument- or value-specific markup.

   if (binding.explicit-defn)
      document-args/vals(binding.explicit-defn, generated-topic, authored-topic)
   end if;
   
   // Create topics for methods in the generic.
   
   for (meth-defn :: <implicit-generic-defn> in binding.implicit-defns)
      let meth = make(<generic-method>, generic: binding, method: meth-defn);
      let meth-topics = make-method-topics(meth);
      topics := concatenate(topics, meth-topics);
   end for;

   values(topics, #[])
end method;


define method make-method-topics (generic-method :: <generic-method>)
=> (topics :: <sequence>, catalog-topics :: <sequence>)
   let binding = generic-method.generic-binding;
   let method-defn = generic-method.method-defn;
   let method-params = method-defn.param-list.req-params;
   let fqn = definition-qualified-name(binding, method-params: method-params);
   let namespace = fqn.enclosing-qualified-name;

   // Create body of generated topic.
   
   let generated-topic = make(<function-doc>,
         generated: #t, existent-api: #t, topic-type: #"method",
         id: canonical-id(binding, method-params: method-params),
         title: canonical-title(binding, method-params: method-params),
         qualified-name: fqn, namespace: namespace,
         source-location: method-defn.source-location,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);

   make-alias-titles(generated-topic, binding);
   let vars = table(<case-insensitive-string-table>, "meth" => generic-method);
   let topics = topics-from-template(#"method-topic", generated-topic, vars);

   // Create authored topics.
   
   let authored-topic = make(<function-doc>,
         generated: #f, existent-api: #t, topic-type: #"method",
         id: canonical-id(binding, method-params: method-params),
         title: canonical-title(binding, method-params: method-params),
         qualified-name: fqn, namespace: namespace,
         source-location: method-defn.source-location,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);

   let authored-topics = make-authored-topics(method-defn.markup-tokens, authored-topic);
   topics := concatenate!(topics, authored-topics);
   
   // Add argument and value documentation from argument- or value-specific markup.

   document-args/vals(method-defn, generated-topic, authored-topic);
   
   values(topics, #[])
end method;


define method document-args/vals
   (defn :: type-union(<explicit-generic-defn>, <implicit-generic-defn>,
                       <explicit-function-defn>),
    generated-topic :: <function-doc>, authored-topic :: <function-doc>)
=> ()
   let arg-items = defn.param-list.all-params/values;
   let arg-defn-list = generated-topic.args-section
         & generated-topic.args-section.defn-list;
   let val-items = defn.value-list.all-params/values;
   let val-defn-list = generated-topic.vals-section
         & generated-topic.vals-section.defn-list;

   if (arg-defn-list)
      replace-list-item-docs(arg-defn-list, arg-items)
   end if;
   if (val-defn-list)
      replace-list-item-docs(val-defn-list, val-items)
   end if;

   // For consistency with the way init-arg docs work, if arguments and
   // values are documented in the main function markup, use that instead of
   // the argument- or value-specific markup.
   if (~arg-items.empty? & authored-topic.args-section)
      let tokens = arg-items.all-markup-tokens;
      unless (tokens.empty?)
         unused-docs-in-topic(location: authored-topic.args-section.source-location,
               doc-locations: map(token-src-loc, tokens).item-string-list);
      end unless;
   end if;
   if (~val-items.empty? & authored-topic.vals-section)
      let tokens = val-items.all-markup-tokens;
      unless (tokens.empty?)
         unused-docs-in-topic(location: authored-topic.vals-section.source-location,
               doc-locations: map(token-src-loc, tokens).item-string-list);
      end unless;
   end if;
end method;


define method all-params/values (param-list :: <param-list>)
=> (params :: <sequence>)
   param-list.req-params
end method;


define method all-params/values (param-list :: <key-param-list>)
=> (params :: <sequence>)
   let params = concatenate(next-method(), param-list.key-params);
   if (param-list.rest-param)
      params := add!(params, param-list.rest-param)
   end if;
   params
end method;


define method all-params/values (param-list :: <var-param-list>)
=> (params :: <sequence>)
   add(next-method(), param-list.rest-param)
end method;


define method all-params/values (value-list :: <value-list>)
=> (vals :: <sequence>)
   let vals = value-list.req-values;
   if (value-list.rest-value)
      vals := add(vals, value-list.rest-value)
   end if;
   vals
end method;
   

define method defn-list-item-label (param :: <req-param>) => (name :: <string>)
   param.local-name
end method;

define method defn-list-item-label (param :: <key-param>) => (name :: <string>)
   format-to-string("%s:", param.symbol)
end method;

define method defn-list-item-label (param :: <rest-param>) => (name :: <string>)
   format-to-string("#rest %s", param.local-name)
end method;


define method defn-list-item-label (val :: <req-value>) => (name :: <string>)
   val.local-name
end method;

define method defn-list-item-label (val :: <rest-value>) => (name :: <string>)
   format-to-string("#rest %s", val.local-name)
end method;


define class <generic-method> (<object>)
   slot generic-binding :: <generic-binding>, required-init-keyword: #"generic";
   slot method-defn :: <implicit-generic-defn>, required-init-keyword: #"method";
end class;


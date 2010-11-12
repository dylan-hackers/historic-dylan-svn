module: topic-resolver
synopsis: Replaces placeholders with actual content.


define method replace-content-placeholders (doc-tree :: <ordered-tree>) => ()
   for (topic :: false-or(<topic>) keyed-by topic-key in doc-tree)
      if (topic)  // Root of doc-tree is #f.
         visit-content-placeholders(topic, replacer,
               topic-key: topic-key, doc-tree: doc-tree)
      end if
   end for;
end method;


define method replacer (object, #key setter, visited, topic-key, doc-tree)
=> (visit-slots? :: <boolean>)
   // Allow recursion in the general case.
   #t
end method;


define method replacer
   (placeholder :: <api-list-placeholder>, #key setter, visited,
    topic-key :: <ordered-tree-key>, doc-tree :: <ordered-tree>)
=> (visit-slots? :: <boolean>)
   let desired-topic-types =
         select (placeholder.api-type)
            #"functions" => #[ #"function", #"generic-function" ];
            #"libraries" => #[ #"library" ];
            #"variables" => #[ #"variable", #"constant" ];
            #"bindings" => #[ #"function", #"generic-function", #"class",
                              #"variable", #"constant", #"macro", #"unbound",
                              #"placeholder" ];
            #"classes" => #[ #"class" ];
            #"modules" => #[ #"module" ];
            #"unbound-names" => #[ #"unbound" ];
            #"macros" => #[ #"macro" ];
         end select;
   let desired-namespace = placeholder.qualified-scope-name;

   local method combine-unique-titles
            (titles-1 :: <sequence>, titles-2 :: <sequence>) /* of <title-seq> */
         => (combined :: <sequence>)
            union(titles-1, titles-2, test: \=)
         end method;
   
   let found-topics = make(<stretchy-vector>);
   for (topic :: false-or(<topic>) in doc-tree)
      if (topic & member?(topic.topic-type, desired-topic-types)) // Root is #f.
         let matching-titles = 
               if (desired-namespace)
                  // If an API is known in a namespace, it will have a title in
                  // that namespace. Take title(s) and note them with this topic.
                  element(topic.titles-in-namespace, desired-namespace, default: #[])
               else
                  // Take all titles for the topic and note them with this topic.
                  reduce(combine-unique-titles, vector(topic.title),
                         topic.titles-in-namespace);
               end if;
         let topic-titles = map(rcurry(pair, topic), matching-titles);
         found-topics := concatenate!(found-topics, topic-titles);
      end if
   end for;
   
   local method make-api-xref (title-topic :: <pair>) => (xref :: <xref>)
            make(<xref>, source-location: placeholder.source-location,
                 target: title-topic.tail, text: title-topic.head);
         end method,

         method title-sorts-first?
            (title-topic-1 :: <pair>, title-topic-2 :: <pair>)
         => (t1-first? :: <boolean>)
            let t1 :: <title-seq> = title-topic-1.head;
            let t2 :: <title-seq> = title-topic-2.head;
            t1.stringify-title < t2.stringify-title
         end method;
         
   let sorted-topics = sort(found-topics, test: title-sorts-first?);
   placeholder.api-xrefs := map(make-api-xref, sorted-topics);
   #f
end method;

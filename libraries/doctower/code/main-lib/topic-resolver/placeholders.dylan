module: topic-resolver
synopsis: Replaces placeholders with actual content.


define method replace-content-placeholders (doc-tree :: <ordered-tree>) => ()
   for (topic :: false-or(<topic>) keyed-by topic-key in doc-tree)
      if (topic)  // Root of doc-tree is #f.
         visit-content-placeholders(topic, replacer, recurse-topic: pair(topic, #t),
               topic-key: topic-key, doc-tree: doc-tree)
      end if
   end for;
end method;


define method replacer
   (object :: <object>, #key setter, recurse-topic, topic-key, doc-tree)
=> (slots? :: <boolean>)
   #t
end method;


define method replacer
   (topic :: <topic>, #key setter, recurse-topic, topic-key, doc-tree)
=> (slots? :: <boolean>)
   // Only allow recursion into initial topic once.
   if (topic == recurse-topic.head & recurse-topic.tail)
      recurse-topic.tail := #f;
      #t
   end if;
end method;


define method replacer
   (placeholder :: <api-list-placeholder>, #key setter :: false-or(<function>),
    topic-key :: <ordered-tree-key>, doc-tree :: <ordered-tree>, recurse-topic)
=> (slots? :: <boolean>)
   let desired-topic-types =
         select (placeholder.api-type)
            #"functions" => #[ #"function", #"generic-function" ];
            #"libraries" => #[ #"library" ];
            #"variables" => #[ #"variable", #"constant" ];
            #"bindings" => #[ #"function", #"generic-function", #"class",
                              #"variable", #"constant", #"macro", #"unbound" ];
            #"classes" => #[ #"class" ];
            #"modules" => #[ #"module" ];
            #"unbound-names" => #[ #"unbound" ];
            #"macros" => #[ #"macro" ];
         end select;
   let desired-namespace = placeholder.qualified-scope-name;

   let found-topics = make(<stretchy-vector>);
   for (topic :: false-or(<topic>) in doc-tree)
      if (topic & member?(topic.topic-type, desired-topic-types)) // Root is #f.
         if (desired-namespace)
            let enclosing-name = topic.fully-qualified-name.enclosing-qualified-name;
            if (enclosing-name = desired-namespace
                  | enclosing-name.enclosing-qualified-name = desired-namespace)
               found-topics := add!(found-topics, topic)
            end if
         else
            found-topics := add!(found-topics, topic)
         end if
      end if
   end for;

   found-topics := sort!(found-topics, test:
         method (topic1 :: <topic>, topic2 :: <topic>) => (before? :: <boolean>)
            topic1.title.stringify-title < topic2.title.stringify-title
         end method);
         
   local method make-list-item (topic :: <api-doc>) => (list-item :: <content-seq>)
            let title-ref = make(<conref>, source-location: placeholder.source-location,
                  target: topic, style: #"title");
            let xref = make(<xref>, source-location: placeholder.source-location,
                  target: topic, text: title-ref);
            let para = make(<paragraph>, source-location: placeholder.source-location,
                  content: markup-seq(xref));
            content-seq(para)
         end method;

   let api-list = make(<unordered-list>, source-location: placeholder.source-location);
   api-list.items := map(make-list-item, found-topics);
   setter(api-list);
   #t
end method;

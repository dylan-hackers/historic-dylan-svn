module: topic-resolver
synopsis: Replaces placeholders with actual content.


define method replace-content-placeholders (doc-tree :: <ordered-tree>) => ()
   // TODO: Line markers and footnotes.
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

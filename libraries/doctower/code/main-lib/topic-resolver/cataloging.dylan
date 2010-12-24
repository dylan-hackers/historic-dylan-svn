module: topic-resolver


//
// Topic look-up information
//


define method resolution-info (topics :: <sequence>)
=> (target-resolutions :: <table>, duplicate-title-targets :: <table>)

   // Collect info.
   
   let combined-ids = make(<case-insensitive-string-table>);
   let titles = make(<case-insensitive-string-table>);
   let fqns = make(<case-insensitive-string-table>);
   for (topic in topics)
      visit-targets(topic, add-target-info, topic: topic,
            ids: combined-ids, titles: titles, fqns: fqns)
   end for;
   
   // Check for duplicates and collate. Resolution priority goes
   // 1) ID, 2) FQN as ID, 3) unique title. Assign to common resolution table
   // in 3-2-1 order so that higher priority resolutions replace lower.
   
   let target-resolutions = make(<case-insensitive-string-table>);
   let dup-titles = make(<case-insensitive-string-table>);
   
   // Titles
   for (target-list keyed-by title in titles)
      debug-assert(target-list.size >= 1, "No targets with title %=", title);
      if (target-list.size = 1)
         target-resolutions[title] := target-list.first
      else
         dup-titles[title] := target-list
      end if
   end for;
   
   // FQNs
   for (target-list keyed-by fqn in fqns)
      debug-assert(target-list.size >= 1, "No targets with fqn %=", fqn);
      if (target-list.size = 1)
         target-resolutions[fqn.qualified-name-as-id] := target-list.first
      else
         let fqn-locs = map(fully-qualified-name-source-loc, target-list);
         multiple-topics-for-fqn(location: fqn-locs.first, fqn: fqn,
               fqn-locations: fqn-locs.item-string-list);
      end if
   end for;
   
   // IDs
   for (target-list keyed-by id in combined-ids)
      debug-assert(target-list.size >= 1, "No targets with id %=", id);
      if (target-list.size = 1)
         target-resolutions[id] := target-list.first;

         // Ensure ID is not also used as title.
         let matching-title-topics = element(titles, id, default: #f);
         if (matching-title-topics)
            let title-locs = map(title-source-loc, target-list);
            id-matches-topic-title(location: target-list.first.id-source-loc,
                  title-locations: title-locs.item-string-list);
         end if
      else
         let id-locs = map(id-source-loc, target-list);
         duplicate-id-in-targets(location: id-locs.first,
               id-locations: id-locs.item-string-list);
      end if
   end for;
   
   values(target-resolutions, dup-titles)
end method;


define method add-target-info
   (object :: <object>, #key setter, visited, topic, ids, fqns, titles)
=> (do-slots? :: <boolean>)
   #t
end method;

define method add-target-info
   (object :: <api-doc>, #key setter, visited, topic, ids, fqns, titles)
=> (do-slots? :: <boolean>)
   add-target-fqn(object, fqns);
   next-method()
end method;

define method add-target-info
   (object :: <topic>, #key setter, visited, topic, ids, fqns, titles)
=> (do-slots? :: <boolean>)
   add-target-id(object, #f, ids);
   add-target-title(object, titles);
   #t
end method;

define method add-target-info
   (object :: <section>, #key setter, visited, topic, ids, fqns, titles)
=> (do-slots? :: <boolean>)
   add-target-id(topic, object, ids);
   add-target-title(object, titles);
   #f
end method;


define method add-target-id
   (topic :: <topic>, section :: <section>, id-table :: <table>)
=> ()
   if (section.id)
      if (~topic.id & section.id.first ~= ':')
         section-id-without-topic-id(location: section.id-source-loc)
      else
         let id-string = format-to-string("%s/%s", topic.id, section.id);
         add-target-for-string(section, id-table, id-string);
      end if
   end if
end method;


define method add-target-id
   (topic :: <topic>, section == #f, id-table :: <table>)
=> ()
   if (topic.id)
      add-target-for-string(topic, id-table, topic.id);
   end if
end method;


define method add-target-title
   (target :: type-union(<topic>, <section>), title-table :: <table>)
=> ()
   let title-string = stringify-title(target.title);
   add-target-for-string(target, title-table, title-string);
end method;


define method add-target-title
   (target :: <api-doc>, title-table :: <table>)
=> ()
   let titles = reduce(concatenate!, vector(target.title), target.titles-in-namespace);
   let title-strings = map(stringify-title, titles);
   do(curry(add-target-for-string, target, title-table), title-strings);
end method;


define method add-target-fqn
   (topic :: <api-doc>, fqn-table :: <table>)
=> ()
   if (topic.fully-qualified-name)
      add-target-for-string(topic, fqn-table, topic.fully-qualified-name);
   end if;
end method;


define method add-target-for-string
   (target, target-table :: <table>, string :: <string>)
=> ()
   let target-list = element(target-table, string, default: #());
   target-list := add-new!(target-list, target);
   target-table[string] := target-list;
end method;


define method sections-by-parm-name (topic :: <topic>) => (parm-table :: <table>)
   make(<case-insensitive-string-table>)
end method;


define method sections-by-parm-name (topic :: <class-doc>)
=> (parm-table :: <table>)
   let parms = next-method();
   add-parms-from-section(parms, topic.keywords-section)
end method;


define method sections-by-parm-name
   (topic :: type-union(<function-doc>, <macro-doc>))
=> (parm-table :: <table>)
   let parms = next-method();
   add-parms-from-section(parms, topic.vals-section);
   add-parms-from-section(parms, topic.args-section)
end method;


define method add-parms-from-section
   (parms :: <table>, section :: false-or(<section>))
=> (parms :: <table>)
   let parm-list = section.section-parm-list;
   if (parm-list)
      for (row from 0 below dimension(parm-list.items, 0))
         let parm-markup = parm-list.items[row, 0];
         let parm-name = parm-markup.stringify-markup.standardize-parm-target;
         parms[parm-name] := section;
      end for
   end if;
   parms
end method;


define method section-parm-list (section :: false-or(<section>))
=> (parm-list :: false-or(<parm-list>))
   if (section)
      let section-content = as(<simple-object-vector>, section.content);
            // BUGFIX: Necessary because choose cannot create a limited sequence on demand.
      let parm-lists = choose(rcurry(instance?, <parm-list>), section-content);
      ~parm-lists.empty? & parm-lists.first;
   end if
end method;


//
// API catalogs
//


define method add-catalog-information (doc-tree :: <ordered-tree>)
=> ()
   for (topic keyed-by topic-key in doc-tree)
      add-catalog-info-to-topic(topic, topic-key, doc-tree)
   end for
end method;


define method add-catalog-info-to-topic
   (topic, topic-key, doc-tree)
=> ()
end method;


define method add-catalog-info-to-topic
   (catalog :: <library-doc>, catalog-key, doc-tree)
=> ()
   catalog.api-xrefs := xrefs-for-catalog(doc-tree, #f, #[ #"module" ],
         catalog.qualified-scope-name);
end method;


define method add-catalog-info-to-topic
   (catalog :: <module-doc>, catalog-key, doc-tree)
=> ()
   catalog.api-xrefs := xrefs-for-catalog(doc-tree, #f,
         #[ #"function", #"generic-function", #"class", #"variable",
            #"constant", #"macro", #"unbound", #"placeholder" ],
         catalog.qualified-scope-name);
end method;


define method add-catalog-info-to-topic
   (catalog :: <catalog-topic>, catalog-key, doc-tree)
=> ()
   let catalog-type = regexp-matches(catalog.id, "^(:[A-Za-z]+)");
   if (catalog-type)
      let desired-topic-types =
            select (catalog-type by case-insensitive-equal?)
               (":Bindings", ":Others")
                  => #[ #"function", #"generic-function", #"class", #"variable",
                        #"constant", #"macro", #"unbound", #"placeholder" ];
               ":Libraries" => #[ #"library" ];
               ":Modules"   => #[ #"module" ];
               ":Functions" => #[ #"function", #"generic-function" ];
               ":Variables" => #[ #"variable", #"constant" ];
               ":Classes"   => #[ #"class" ];
               ":Macros"    => #[ #"macro" ];
               ":Unbound"   => #[ #"unbound" ];
            end select;
      let parent-key =
            if (case-insensitive-equal?(catalog-type, ":Others")) catalog-key end;
      catalog.api-xrefs := xrefs-for-catalog(doc-tree, parent-key,
            desired-topic-types, catalog.qualified-scope-name);
   end if
end method;


define method xrefs-for-catalog
   (doc-tree :: <ordered-tree>, catalog-key :: false-or(<ordered-tree-key>),
    desired-topic-types :: <sequence>, desired-namespace :: false-or(<string>))
=> (xrefs :: <sequence>)
   let candidate-topic-keys =
         if (catalog-key)
            catalog-key.inf-key-sequence
         else
            remove(doc-tree.key-sequence, doc-tree.root-key,
                   test: doc-tree.key-test, count: 1)
         end if;
            
   local method combine-unique-titles
            (titles-1 :: <sequence>, titles-2 :: <sequence>) /* of <title-seq> */
         => (combined :: <sequence>)
            union(titles-1, titles-2, test: \=)
         end method,
         
         method in-desired-namespace? (candidate :: <string>) => (desired? :: <boolean>)
            begins-with-string?(candidate, desired-namespace,
                                string-test: case-insensitive-equal?)
         end method;
   
   let found-topics = make(<stretchy-vector>) /* of (title, topic) pairs */;
   for (topic-key in candidate-topic-keys)
      let topic :: <topic> = doc-tree[topic-key];
      if (member?(topic.topic-type, desired-topic-types))
         // If an API is known in a namespace, it will have a title in that
         // namespace. Take title(s) and note them with this topic.
         let namespace-keys = topic.titles-in-namespace.key-sequence;
         let matching-keys =
               if (desired-namespace)
                  choose(in-desired-namespace?, namespace-keys)
               else
                  namespace-keys
               end if;
         unless (matching-keys.empty?)
            let matching-title-groups =
                  map(curry(element, topic.titles-in-namespace), matching-keys);
            let matching-titles =
                  reduce(combine-unique-titles, make(<stretchy-vector>),
                         matching-title-groups);
            let topic-title-pairs = map(rcurry(pair, topic), matching-titles);
            found-topics := concatenate!(found-topics, topic-title-pairs);
         end unless
      end if
   end for;
   
   local method make-api-xref (title-topic :: <pair>) => (xref :: <xref>)
            make(<xref>, source-location: $generated-source-location,
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
   map(make-api-xref, sorted-topics);
end method;

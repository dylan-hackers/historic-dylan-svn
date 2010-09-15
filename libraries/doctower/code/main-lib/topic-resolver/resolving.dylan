module: topic-resolver


/** 
Synopsis: Verifies that all IDs are unique and that no ID duplicates a topic
title.
**/
define method check-topic-ids
   (topics :: <sequence>,
    #key id-table: defined-ids :: <table>, title-table: defined-titles :: <table>)
=> ()
   for (id in defined-ids.key-sequence)
      let topic-by-title = element(defined-titles, id, default: #f);
      if (topic-by-title)
         let topic-by-id :: <topic> = defined-ids[id];
         id-matches-topic-title(location: topic-by-id.id-source-loc,
                                title-location: topic-by-title.title-source-loc);
      end if;
   end for;
end method;


/**
Synopsis: Resolves all xref link placeholders.

This method resolves xref targets, including arguments and values.
**/
define method resolve-xref-placeholders
   (topics :: <sequence>,
    #key id-table: defined-ids :: <table>, fqn-table: defined-fqns :: <table>,
    title-table: defined-titles :: <table>)
=> (topics :: <sequence>)
   for (topic in topics)
      let defined-parms = sections-by-parm-name(topic);
      visit-xrefs(topic, resolve-xref-in-topic, topic: topic, titles: defined-titles,
                  ids: defined-ids, fqns: defined-fqns, parms: defined-parms);
   end for;
   topics
end method;


define method resolve-xref-in-topic
   (object :: <object>, #key setter, topic: current-topic, titles, ids, fqns, parms)
=> (visit-slots? :: <boolean>)
   // Allow recursion in the general case.
   #t
end method;


define method resolve-xref-in-topic
   (topic :: <topic>, #key setter, topic: current-topic, titles, ids, fqns, parms)
=> (visit-slots? :: <boolean>)
   // Only allow recursion into current topic.
   topic == current-topic
end method;


define method resolve-xref-in-topic
   (xref :: <xref>, #key setter, topic: current-topic, titles, ids, fqns, parms)
=> (visit-slots? :: <boolean>)
   if (instance?(xref.target, <target-placeholder>))
      let placeholder :: <target-placeholder> = xref.target;
      let (resolution, replace-text-with-title?) =
            begin
               let local-res = resolve-local-link(placeholder, current-topic, parms);
               if (local-res)
                  values(local-res, #f)
               else
                  values(resolve-topic-link(placeholder, current-topic, titles, ids, fqns),
                         xref.target-from-text?)
               end if
            end;

      if (resolution)
         xref.target := resolution;
         if (replace-text-with-title?)
            xref.text := make(<conref>, target: resolution, style: #"title",
                  source-location: placeholder.source-location)
         end if

      elseif (xref.target-from-text?)
         // May not be intended as an actual link target. Give warning and
         // remove unlinkable xref.
         unresolvable-target-in-link(location: placeholder.source-location,
                target-text: placeholder.target);
         setter(xref.text);

      else
         // Explicit link target. Give error.
         target-not-found-in-link(location: placeholder.source-location,
                target-text: placeholder.target);
      end if
   end if;
   #t
end method;


/**
Synopsis: Resolves topic and table-of-contents-related placeholders.

This method resolves all remaining <target-placeholder> objects.

It discards catalog topics (such as "All Libraries" or "Modules in Io") that are
not actually referenced.

This method ensures IDs are unique as a by-product.

--- Conditions: ---
Signals an error if a link fails to resolve.
**/
define method resolve-topic-placeholders
   (topics :: <sequence>, tables-of-content :: <sequence>, catalog-topics :: <sequence>,
    #key id-table: defined-ids :: <table>, fqn-table: defined-fqns :: <table>,
    title-table: defined-titles :: <table>, dup-title-table: dup-titles :: <table>)
=> (resolved-topics :: <sequence>, tables-of-content :: <sequence>)
   let unused-catalogs = catalog-topics.copy-sequence;
   
   // Resolution method.
   local method resolve (object, #key setter, topic: current-topic :: false-or(<topic>))
         => (slots? :: <boolean>)
            select (object by instance?)
               <target-placeholder> =>
                  let link :: <target-placeholder> = object;
                  let topic-found = resolve-topic-link(link, current-topic,
                        defined-titles, defined-ids, defined-fqns);
                  if (topic-found)
                     setter(topic-found);
                     unused-catalogs := remove!(unused-catalogs, topic-found);
                  elseif (key-exists?(dup-titles, link.target))
                     let locs = map(source-location, dup-titles[link.target]);
                     ambiguous-title-in-link(location: link.source-location,
                           target-text: link.target,
                           topic-locations: locs.item-string-list);
                  else
                     target-not-found-in-link(location: link.source-location,
                            target-text: link.target);
                  end if;
                  #t;
               <topic> =>
                  // Only allow recursion into current topic.
                  object == current-topic;
               otherwise =>
                  // Allow recursion into everything else.
                  #t;
            end select
         end method;
            
   // Assign topics in place of placeholders.
   for (topic in topics)
      visit-target-placeholders(topic, resolve, topic: topic)
   end for;

   // Assign topics in place of tables of content references.
   for (toc in tables-of-content)
      for (toc-ref :: false-or(<topic-ref>) in toc)
         if (toc-ref)
            resolve(toc-ref.target, setter: rcurry(target-setter, toc-ref),
                    topic: #f);
         end if;
      end for;
   end for;
   
   // Remove unused catalog topics.
   let topics = reduce(remove!, topics, unused-catalogs);

   values(topics, tables-of-content);
end method;


/// Synopsis: Makes a table of topics keyed by ID.
/// Conditions: Signals error if several topics duplicate an ID.
///
define method topics-by-id (topics :: <sequence>) => (id-table :: <table>)
   let id-topics = choose(id, topics);
   let id-table = make(<case-insensitive-string-table>, size: id-topics.size);
   for (topic in id-topics)
      let dup-id-topics =
            choose(compose(curry(case-insensitive-equal?, topic.id), id), id-topics);
      when (dup-id-topics.size > 1)
         duplicate-id-in-topics(location: topic.id-source-loc,
               id-locations: item-string-list(map(id-source-loc, dup-id-topics)) |
                             "various locations");
      end when;
      id-table[topic.id] := topic
   end for;
   id-table
end method;


/// Synopsis: Makes a table of topics keyed by fully qualified name.
///
define method topics-by-fqn (topics :: <sequence>) => (fqn-table :: <table>)
   let fqn-topics = choose(conjoin(rcurry(instance?, <api-doc>), fully-qualified-name),
                           topics);
   let fqn-table = make(<case-insensitive-string-table>, size: fqn-topics.size);
   for (topic in fqn-topics)
      fqn-table[topic.fully-qualified-name.qualified-name-as-id] := topic
   end for;
   fqn-table
end method;


/// Synopsis: Makes a table of topics keyed by topic title.
///
/// Topic titles need to be stringified because links to titles are strings and
/// can't use images or styled text. Topics with identical titles are returned
/// in a separate table.
///
/// Values:
///   unique-title-table     - A <table> keyed by title containing topics.
///   duplicate-title-table  - A <table> keyed by title containing topic lists.
///
define method topics-by-title (topics :: <sequence>)
=> (unique-title-table :: <table>, duplicate-title-table :: <table>)
   let title-table = make(<case-insensitive-string-table>, size: topics.size);
   let dup-titles = list();
   for (topic in topics)
      let string-title = stringify-title(topic.title);
      let topic-list = element(title-table, string-title, default: #());
      title-table[string-title] := add(topic-list, topic);
   end for;
   
   // Effectively a "choose size of 1," but choose does not work on tables.
   let unique-title-table = make(<case-insensitive-string-table>, size: topics.size);
   let removed-titles = list();
   for (topic-list keyed-by topic-title in title-table)
      if (topic-list.size = 1)
         unique-title-table[topic-title] := topic-list.first;
         removed-titles := add!(removed-titles, topic-title);
      end if;
   end for;

   do (curry(remove-key!, title-table), removed-titles);

   values(unique-title-table, title-table)
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


// TODO: Implement linking to arbitrary sections.

/// Synopsis: Determines what topic a link refers to.
///
/// In general, a link is resolved to one of the following, in order:
///   1. Local argument/value name
///   2. ID
///   3. Fully qualified name disguised as ID
///   4. Unique title
///   5. API in current module/library
///   6. Unique API in other module/library
///
/// Case #1 is handled in 'resolve-xref-placeholders' by calling
/// 'resolve-local-link' before this method, and not handled at all in
/// 'resolve-topic-placeholders'.
///
/// Arguments:
///   link              -  The <target-placeholder> to resolve.
///   containing-topic  -  The topic containing the link, used to look up the
///                        current module/library.
///   topics-by-id      -  Topics indexed by id.
///   topics-by-fqn     -  Topics indexed by fully qualified name disguised as
///                        id.
///   topics-by-title   -  Topics indexed by unique title (in string form).
///
/// Values:
///   resolution  -  #f if link could not be resolved, else the <topic> it
///                  resolved to.
///
define method resolve-topic-link
   (link :: <target-placeholder>, containing-topic :: false-or(<topic>),
    topics-by-title :: <table>, topics-by-id :: <table>, topics-by-fqn :: <table>)
=> (resolution :: false-or(<topic>))
   let topic = element(topics-by-id, link.target, default: #f) |
               element(topics-by-fqn, link.target, default: #f) |
               element(topics-by-title, link.target, default: #f);
   if (~topic)
      // It is an API or a duplicate title or something unknown.
      // TODO: API canonicalization and lookup.
   end if;
   topic
end method;


define method resolve-local-link
   (link :: <target-placeholder>, containing-topic :: false-or(<topic>),
    section-by-parm-name :: <table>)
=> (resolution :: false-or(<section>))
   let link-text = link.target.standardize-target;
   element(section-by-parm-name, link-text, default: #f)
end method;


define method standardize-target (target :: <string>) => (cleaned :: <string>)
   let cleaned = target.copy-sequence;
   cleaned := replace-elements!(cleaned, rcurry(\=, '\n'), always(' '));
   cleaned := regexp-replace(cleaned, " {2,}", " ");
   cleaned := regexp-replace(cleaned, "^#[^ ]+ ", "");
   cleaned := regexp-replace(cleaned, ":$", "");
   cleaned
end method;


define method add-parms-from-section
   (parms :: <table>, section :: false-or(<section>))
=> (parms :: <table>)
   let parm-list = section.section-parm-list;
   if (parm-list)
      for (row from 0 below dimension(parm-list.items, 0))
         let parm-markup = parm-list.items[row, 0];
         let parm-name = parm-markup.stringify-markup.standardize-target;
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


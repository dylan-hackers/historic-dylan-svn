module: topic-resolver

/**
Synopsis: Resolves topic and table-of-contents-related placeholders.

This method resolves placeholders from Section, quoted VI, Relevant To, See
Also, Synopsis Of, Contents Of, and Ditto directives. It does not deal with
quoted QV directives, because those may be referring to local Arguments or
Values entries, which can't be resolved until the topic is finalized.

It discards catalog topics (such as "All Libraries" or "Modules in Io") that are
not actually referenced.

This method ensures IDs are unique as a by-product.

--- Conditions: ---
Signals an error if two topics have the same ID or a link fails to resolve.
**/
define method resolve-topic-placeholders
   (topics :: <sequence>, tables-of-content :: <sequence>, catalog-topics :: <sequence>)
=> (resolved-topics :: <sequence>, tables-of-content :: <sequence>)
   let defined-ids = topics-by-id(topics);
   let defined-fqns = topics-by-fqn(topics);
   let (defined-titles, dup-titles) = topics-by-title(topics);
   let unused-catalogs = catalog-topics.copy-sequence;
   
   // Ensure no id duplicates a topic title.
   for (id in defined-ids.key-sequence)
      let topic-by-title = element(defined-titles, id, default: #f);
      if (topic-by-title)
         let topic-by-id :: <topic> = defined-ids[id];
         id-matches-topic-title(location: topic-by-id.id-source-loc,
                                title-location: topic-by-title.title-source-loc);
      end if;
   end for;
   
   // Resolution method.
   local method resolve (link :: <target-placeholder>, #key setter, topic) => ()
            let topic-found = resolve-link
                  (link, topic, defined-titles, defined-ids, defined-fqns);
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
            end if
         end method;
            
   // Assign topics in place of placeholders.
   for (topic in topics)
      visit-topic-placeholders(topic, resolve, topic: topic)
   end for;

   // Assign topics in place of tables of content references.
   for (toc in tables-of-content)
      for (toc-ref :: false-or(<topic-ref>) in toc)
         when (toc-ref)
            let link = toc-ref.target;
            if (instance?(link, <target-placeholder>))
               resolve(link, setter: rcurry(target-setter, toc-ref), topic: #f);
            end if;
         end when;
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


/// Synopsis: Determines what topic a link refers to.
///
/// In general, a link is resolved to one of the following, in order:
///   1. Local argument/value name
///   2. ID
///   3. Fully qualified name disguised as ID
///   4. Unique title
///   5. API in current module/library
///   6. Unique API in other module/library
/// At this point in the code, though, we are only processing topic-level stuff,
/// so we won't see any #1.
///
/// Arguments:
///   link              -  The <target-placeholder> to resolve.
///   containing-topic  -  The topic containing the link, used to look up the
///                        current module/library.
///   topics-by-id      -  Topics indexed by id.
///   topics-by-fqn     -  Topics indexed by fully qualified name disguised as
///                        id.
///   topics-by-title   -  Topics indexed by unique title (in string form).
/// Values:
///   resolution  -  #f if link could not be resolved, else the <topic> it
///                  resolved to.
///
define method resolve-link
   (link :: <target-placeholder>, containing-topic :: false-or(<topic>),
    topics-by-title :: <table>, topics-by-id :: <table>, topics-by-fqn :: <table>)
=> (resolution :: false-or(<topic>))
   let topic = element(topics-by-id, link.target, default: #f) |
               element(topics-by-fqn, link.target, default: #f) |
               element(topics-by-title, link.target, default: #f);
   if (~topic)
      // It is an API or a duplicate title. It will not be an argument/value
      // because those can only be legitimately found in an <xref>, which we are
      // not processing.
      // TODO: API canonicalization and lookup.
   end if;
   topic
end method;

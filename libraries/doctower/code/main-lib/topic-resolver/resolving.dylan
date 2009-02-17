module: topic-resolver

/**
Synopsis: Resolves topic and table-of-contents-related placeholders.

This method resolves placeholders from Section, quoted VI, Relevant To, See
Also, Synopsis Of, Contents Of, and Ditto directives. It does not deal with
quoted QV directives, because those may be referring to local Arguments or
Values entries, which can't be resolved until the topic is finalized.

This method does not know which topics are duplicates of each other and need
to be merged vs. totally different topics that happen to share a name. The
topics need to be arranged in the table of contents before that can happen.
Also, two topics that need to be merged may have a different ID on each topic;
this is an error that must be caught after the topics have been arranged.

If a link is to a duplicate title, it won't be resolved, so there may still be
<target-placeholders> after this method completes.

This method ensures IDs are unique as a by-product.

--- Conditions: ---
Signals an error if two topics have the same ID or a link fails to resolve.
**/
define method resolve-topic-placeholders
   (topics :: <sequence>, tables-of-content :: <sequence>)
=> (resolved-topics :: <sequence>, ambiguous-topic-table :: <table>,
    tables-of-content :: <sequence>)
   let defined-ids = topics-by-id(topics);
   let (defined-titles, dup-titles) = topics-by-title(topics);
   
   // Ensure no id duplicates a topic title.
   for (id in defined-ids.key-sequence)
      let topic-by-title = element(defined-titles, id, default: #f);
      if (topic-by-title)
         let topic-by-id :: <topic> = defined-ids[id];
         id-matches-topic-title(location: topic-by-id.id-source-loc,
                                title-location: topic-by-title.title-source-loc);
      end if;
   end for;
   
   visit-placeholders(
         topics,
         method (link :: <target-placeholder>, #key setter) => ()
            let topic = resolve-link(link, defined-titles, defined-ids);
            if (topic)
               setter(topic)
            elseif (~key-exists?(dup-titles, link.target))
               target-not-found-in-link(location: link.source-location,
                                        target-text: link.target);
            end if;
         end);

   for (toc in tables-of-content)
      for (placeholder :: false-or(<topic-ref>) in toc)
         when (placeholder)
            let link = placeholder.target;
            let topic = resolve-link(link, defined-titles, defined-ids);
            if (topic)
               placeholder.target := topic;
            else
               target-not-found-in-link(location: link.source-location,
                                        target-text: link.target);
            end if;
         end when;
      end for;
   end for;

   values(topics, dup-titles, tables-of-content);
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


/// Synopsis: Makes a table of topics keyed by topic title.
/// Discussion: Topic titles need to be stringified because links to titles
/// are strings and can't use images or styled text. Topics with identical
/// titles aren't included; those links can't be resolved at this time.
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
///   1. Local attribute/value name
///   2. IDs
///   3. Unique title
///   4. API in current module/library
///   5. Unique API in other module/library
/// At this point in the code, though, we are only dealing with topic-level stuff,
/// so we won't see any #1. Also at this point, topics haven't been merged
/// or placed, so no point in worrying about uniqueness.
///
/// Values:
///   resolution  -  #f if link could not be resolved, else the <topic> it
///                  resolved to.
///
define method resolve-link
   (link :: <target-placeholder>, topics-by-title :: <table>,
    topics-by-id :: <table>)
=> (resolution :: false-or(<topic>))
   let topic = element(topics-by-id, link.target, default: #f) |
               element(topics-by-title, link.target, default: #f);
   if (~topic)
      // It is an API, a duplicate title, or an argument/value. APIs need to be
      // canonicalized and looked up; arguments/values need to be resolved later.
      // TODO: API canonicalization and lookup.
   end if;
   topic
end method;


/// Generic Function: visit-placeholders
/// Synopsis: Visits a <topic> and its nested elements that can contain
/// <target-placeholder> objects.
///
/// Arguments:
///   element     - The <markup-element> to visit.
///   operation   - A <function> on 'element'. The function is passed a
///                 setter: argument.

define collection-recursive slot-visitor visit-placeholders
   <bold>,                 text;
   <cite>,                 text;
   <class-doc>,            content, shortdesc, parent, see-also, relevant-to,
                           keywords-section;
   <code-phrase>,          text;
   <conref>,               target;
   <defn-list>,            items;
   <ditto-placeholder>,    target;
   <emphasis>,             text;
   <footnote>,             content;
   <function-doc>,         content, shortdesc, parent, see-also, relevant-to,
                           args-section, vals-section, conds-section;
   <italic>,               text;
   <macro-doc>,            content, shortdesc, parent, see-also, relevant-to,
                           args-section, vals-section;
   <ordered-list>,         items;
   <paragraph>,            content;
   <section>,              title, content;
   <simple-table>,         headings, items;
   <target-placeholder>,   ;
   <term-style>,           text;
   <term>,                 text;
   <vi-xref>,              target;
   <topic>,                content, shortdesc, parent, see-also, relevant-to;
   <topic-ref>,            target;
   <underline>,            text;
   <unordered-list>,       items;
end slot-visitor;

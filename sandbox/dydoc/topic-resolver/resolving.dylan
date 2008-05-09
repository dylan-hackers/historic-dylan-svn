module: topic-resolver


/// Synopsis: Resolves topic and table-of-contents-related placeholders.
///
/// This method resolves placeholders from Section, quoted ToC, Relevant To, See
/// Also, Synopsis Of, Contents Of, and Ditto directives. It does not deal with
/// quoted QV directives, because those may be referring to local Arguments or
/// Values entries, which can't be resolved until the topic is finalized.
/// 
/// This method does not know which topics are duplicates of each other and 
/// need to be merged vs. totally different topics that happen to share a name.
/// The topics need to be arranged in the table of contents before that can happen.
/// Also, two topics that need to be merged may have a different ID on each topic;
/// this is an error that must be caught after the topics have been arranged.
///
/// If a link is to a duplicate title, it won't be resolved, so there may still
/// be <target-placeholders> after this method completes.
/// 
/// This method ensures IDs are unique as a by-product.
/// 
/// Conditions:
///   Signals an error if two topics have the same ID or a link fails to resolve.
///
define method resolve-topic-placeholders (topics :: <sequence>)
=> (resolved-topics :: <sequence>)
   let defined-ids = topics-by-id(topics);
   let (defined-titles, dup-titles) = topics-by-title(topics);
   // resolve-placeholders(topics, setter: #f,
   //                      id-topics: defined-ids,
   //                      title-topics: defined-titles,
   //                      dup-titles: dupe-titles);
   visit-placeholders(
         topics,
         method (link :: <target-placeholder>, #key setter) => ()
            let topic = resolve-link(link, defined-titles, defined-ids);
            if (topic)
               setter(topic)
            elseif (~member?(link.target, dup-titles, test: case-insensitive-equal))
               error("Topic or tag \"%s\" not found in link at %s",
                     link.target, link.element-source);
            end if;
         end);
   topics;
end method;


/// Synopsis: Makes a table of topics keyed by ID.
/// Conditions: Signals error if several topics duplicate an ID.
///
define method topics-by-id (topics :: <sequence>) => (id-table :: <table>)
   let id-topics = choose(id, topics);
   let id-table = make(<case-insensitive-string-table>, size: id-topics.size);
   for (topic in id-topics)
      let dup-id-topics =
            choose(compose(curry(case-insensitive-equal, topic.id), id), id-topics);
      when (dup-id-topics.size > 1)
         let source-as-text = compose(curry(format-to-string, "%s"), element-source);
         error("Several topics use the tag \"%s\": %s", topic.id,
               apply(join, ", ", map(source-as-text, dup-id-topics)))
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
=> (title-table :: <table>, duplicate-titles :: <sequence>)
   let title-table = make(<case-insensitive-string-table>, size: topics.size);
   let dup-titles = list();
   for (topic in topics)
      let string-title = stringify-title(topic.title);
      unless (member?(string-title, dup-titles, test: case-insensitive-equal))
         if (key-exists?(title-table, string-title))
            dup-titles := add!(dup-titles, string-title);
            remove-key!(title-table, string-title);
         else
            title-table[string-title] := topic;
         end if;
      end unless;
   end for;
   values(title-table, dup-titles)
end method;


/*
/// Generic Function: resolve-placeholders
/// Synopsis: Replaces <target-placeholder>s in objects with a real topic.
/// Arguments:
///   object        - A object in which to resolve <target-placeholder>s.
///   setter:       - A function taking one argument that can replace the
///                   <target-placeholder> with another object.
///   id-topics:    - A table of <topic> keyed by id string.   
///   title-topics: - A table of <topic> keyed by title string.
///   dup-titles:   - A sequence of title strings used in more that one place.
///
define generic resolve-placeholders
   (object, #key setter, id-topics, title-topics, dup-titles)
=> ();

/// Actually do the resolution.
define method resolve-placeholders
   (link :: <target-placeholder>, #key setter, id-topics, title-topics, dup-titles)
=> ()
   let topic = resolve-link(link, title-topics, id-topics);
   if (topic)
      setter(topic)
   elseif (~member?(link.target, dup-titles, test: case-insensitive-equal))
      error("Topic or tag \"%s\" not found in link at %s",
            link.target, link.element-source);
   end if;
end method;

/// Recurses into sequences, markup sequences, and arrays. <title-markup> won't
/// have any <target-placeholder>s.
define method resolve-placeholders
   (seq :: <collection>, #key setter, id-topics, title-topics, dup-titles)
=> ()
   for (o keyed-by i in seq)
      resolve-placeholders(o, setter: rcurry(element-setter, seq, i),
                           id-topics, title-topics, dup-titles);
   end for;
end method;

/// Nothing to do for most objects.
define method resolve-placeholders
   (obj :: <object>, #key setter, id-topics, title-topics, dup-titles)
=> ()
end method;

define method resolve-placeholders
   (topic :: <topic>, #key setter, id-topics, title-topics, dup-titles)
=> ()
   resolve-placeholders(topic.content, setter: rcurry(content-setter, topic),
                        id-topics, title-topics, dup-titles);
   resolve-placeholders(topic.shortdesc, setter: rcurry(shortdesc-setter, topic),
                        id-topics, title-topics, dup-titles);
   resolve-placeholders(topic.parent, setter: rcurry(parent-setter, topic),
                        id-topics, title-topics, dup-titles);
   resolve-placeholders(topic.see-also, setter: rcurry(see-also-setter, topic),
                        id-topics, title-topics, dup-titles);
   resolve-placeholders(topic.relevant-to, setter: rcurry(relevant-to-setter, topic),
                        id-topics, title-topics, dup-titles);
end method;

define method resolve-placeholders
   (topic :: <class-doc>, #key setter, id-topics, title-topics, dup-titles)
=> ()
   next-method();
   resolve-placeholders(topic.keywords-section,
                        setter: rcurry(keywords-section-setter, topic),
                        id-topics, title-topics, dup-titles);
end method;

define method resolve-placeholders
   (topic :: <function-doc>, #key setter, id-topics, title-topics, dup-titles)
=> ()
   next-method();
   resolve-placeholders(topic.args-section,
                        setter: rcurry(args-section-setter, topic),
                        id-topics, title-topics, dup-titles);
   resolve-placeholders(topic.vals-section,
                        setter: rcurry(vals-section-setter, topic),
                        id-topics, title-topics, dup-titles);
   resolve-placeholders(topic.conds-section,
                        setter: rcurry(conds-section-setter, topic),
                        id-topics, title-topics, dup-titles);
end method;

define method resolve-placeholders
   (topic :: <function-doc>, #key setter, id-topics, title-topics, dup-titles)
=> ()
   next-method();
   resolve-placeholders(topic.args-section,
                        setter: rcurry(args-section-setter, topic),
                        id-topics, title-topics, dup-titles);
   resolve-placeholders(topic.vals-section,
                        setter: rcurry(vals-section-setter, topic),
                        id-topics, title-topics, dup-titles);
end method;

/// Recurses into quote elements.
define method resolve-placeholders
   (obj :: type-union(<bold>, <cite>, <code-phrase>, <emphasis>, <italic>,
                      <term-style>, <term>, <underline>),
    #key setter, id-topics, title-topics, dup-titles)
=> ()
   resolve-placeholders(obj.text, setter: rcurry(text-setter, obj),
                        id-topics, title-topics, dup-titles);
end method;

/// Recurses into list elements.
define method resolve-placeholders
   (obj :: type-union(<defn-list>, <ordered-list>, <unordered-list>),
    #key setter, id-topics, title-topics, dup-titles)
=> ()
   resolve-placeholders(obj.items, setter: rcurry(items-setter, obj),
                        id-topics, title-topics, dup-titles);
end method;

define method resolve-placeholders
   (obj :: <simple-table>, #key setter, id-topics, title-topics, dup-titles)
=> ()
   resolve-placeholders(obj.headings, setter: rcurry(headings-setter, obj),
                        id-topics, title-topics, dup-titles);
   resolve-placeholders(obj.items, setter: rcurry(items-setter, obj),
                        id-topics, title-topics, dup-titles);
end method;

/// Recurses into elements with targets (except for <target-placeholder>).
define method resolve-placeholders
   (obj :: type-union(<conref>, <ditto-placeholder>, <toc-xref>),
    #key setter, id-topics, title-topics, dup-titles)
=> ()
   resolve-placeholders(obj.target, setter: rcurry(target-setter, obj),
                        id-topics, title-topics, dup-titles);
end method;

/// Recurses into elements with content.
define method resolve-placeholders
   (obj :: type-union(<paragraph>, <footnote>),
    #key setter, id-topics, title-topics, dup-titles)
=> ()
   resolve-placeholders(obj.content, setter: rcurry(content-setter, obj),
                        id-topics, title-topics, dup-titles);
end method;

define method resolve-placeholders
   (obj :: <section>, #key setter, id-topics, title-topics, dup-titles)
=> ()
   resolve-placeholders(obj.title, setter: rcurry(title-setter, obj),
                        id-topics, title-topics, dup-titles);
   resolve-placeholders(obj.content, setter: rcurry(content-setter, obj),
                        id-topics, title-topics, dup-titles);
end method;
*/


/// Synopsis: Determines what topic a link refers to.
///
/// In general, a link is resolved to one of the following, in order:
///   1. Local attribute/value name
///   #. IDs
///   #. Unique title
///   #. API in current module/library
///   #. Unique API in other module/library
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
///   element     - The <interm-element> to visit.
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
   <toc-xref>,             target;
   <topic>,                content, shortdesc, parent, see-also, relevant-to;
   <underline>,            text;
   <unordered-list>,       items;
end slot-visitor;

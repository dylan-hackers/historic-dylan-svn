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
   let unknown-links = #();
   visit-placeholders(topics,
         method (placeholder)
            when (instance?(placeholder, <target-placeholder>))
               unknown-links := add!(unknown-links, placeholder)
            end when
         end method);
   
   for (link in unknown-links)
      let topic = resolve-link(link, defined-titles, defined-ids);
      if (topic)
         fix-link(link, topic)
      elseif (~member?(link.target, dup-titles, test: case-insensitive-equal))
         error("Topic or tag \"%s\" not found in link at %s",
               link.target, link.element-source);
      end if;
   end for;
   topics
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


/// General function: fix-link
/// Synopsis: Replaces a <target-placeholder> with a real target.
define generic fix-link (object :: <interm-element>, topic :: <topic>, #key) => ();

/// This method is the basic callee.
define method fix-link (link :: <target-placeholder>, topic :: <topic>, #key) => ()
   fix-link(link.element-owner, topic, link: link)
end method;

/// This method is for all intermediate elements that have a 'target' slot
/// containing the link.
define method fix-link (owner :: <interm-element>, topic :: <topic>, #key link) => ()
   owner.target := topic
end method;

/// <topic> doesn't have a 'target' slot, but has 'relevant-to', 'see-also',
/// and 'parent'. See which of them has the placeholder.
define method fix-link (owner :: <topic>, topic :: <topic>, #key link) => ()
   let parent? = (owner.parent = link);
   let see-also-key = find-key(owner.see-also, curry(\=, link));
   let relevant-to-key = find-key(owner.relevant-to, curry(\=, link));
   if (parent?) owner.parent := topic end if;
   if (see-also-key) owner.see-also[see-also-key] := topic end if;
   if (relevant-to-key) owner.relevant-to[relevant-to-key] := topic end if;
end method;


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

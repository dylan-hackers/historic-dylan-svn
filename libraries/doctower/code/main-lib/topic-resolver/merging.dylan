module: topic-resolver
synopsis: This file merges API topic fragments.


/// Synopsis: Group topics that can be merged together.
// TODO: Maybe merge implicit generic functions with their single methods?
define method group-mergeable-topics (topics :: <sequence>)
=> (grouped-topics :: <sequence>)
   let api-topics = choose(rcurry(instance?, <api-doc>), topics);
   assign-fully-qualified-names(api-topics);
   group-elements(topics, test: test-combinable);
end method;


define method test-combinable (a :: <topic>, b :: <topic>)
=> (combinable? :: <boolean>)
   #f
end method;

define method test-combinable (a :: <api-doc>, b :: <api-doc>)
=> (combinable? :: <boolean>)
   if (a == b)
      #t
   elseif (~a.fully-qualified-name | ~b.fully-qualified-name)
      #f
   elseif (a.fully-qualified-name = b.fully-qualified-name)
      a.object-class = <unbound-doc> | b.object-class = <unbound-doc>
            | a.object-class = b.object-class
   else
      #f
   end if
end method;


/**
Synopsis: Fill in fully qualified names where not provided.

Arguments:
topics - A <sequence> of <api-topic>.
**/
define method assign-fully-qualified-names (api-topics :: <sequence>) => ()
   let (identified-topics, unidentified-topics) =
         partition(fully-qualified-name, api-topics);
   let identified-topic-titles = map(compose(stringify-title, title), identified-topics);
   for (topic :: <api-doc> in unidentified-topics)
      let topic-title = stringify-title(topic.title);
      let matching-topics = choose-by(curry(case-insensitive-equal?, topic-title),
                                      identified-topic-titles, identified-topics);

      if (matching-topics.empty?)
         api-not-found-in-code(location: topic.source-location,
               topic-type: printed-topic-type(topic.topic-type),
               title: topic-title)

      else
         let qualified-name = matching-topics.first.fully-qualified-name;
         if (every?(compose(curry(\=, qualified-name), fully-qualified-name),
                    matching-topics))
            topic.fully-qualified-name := qualified-name
         else
            let locs = map(source-location, matching-topics).item-string-list;
            ambiguous-api-in-topics(location: topic.source-location,
                  title: topic-title, topic-locations: locs);
         end if;
      end if;
   end for
end method;


/**
Synopsis: Combine a series of partial authored and generated topics into one,
and perform validity checks that require both.

Specifically, this function checks the authored argument, value, and keyword
sections against the automatically-generated ones, and warns if a fully
qualified name is given for a non-existent API.

Arguments:
topics   - A sequence of <topic>. If there is more than one topic, then all
         topics are <api-topic> and have identical fully qualified names. If
         there is only one topic, then it may or may not be an <api-topic> and
         may or may not have a fully qualified name.
**/
define method check-and-merge-topics (topics :: <sequence>) => (topic :: <topic>)
   // Warn if non-existent API has fully qualified name.
   if (instance?(topics.first, <api-doc>))
      if (topics.first.fully-qualified-name & every?(complement(existent-api?), topics))
         fully-qualified-name-not-found-in-code
               (location: topics.first.fully-qualified-name-source-loc,
                qualified-name: topics.first.fully-qualified-name)
      end if
   end if;
   
   if (topics.size = 1)
      topics.first
   else
      // Ensure that there is no more than one user-authored topic.
      let (generated-topics, authored-topics) = partition(generated-topic?, topics);
      if (authored-topics.size > 1)
         let first-topic = authored-topics.first;
         let locs = map(source-location, authored-topics).item-string-list;
         multiple-topics-for-api(location: first-topic.source-location,
               name: stringify-title(first-topic.title), topic-locations: locs)
      end if;
      
      // Preferentially keep authored topics because they have useful source
      // locations and better ids and authored sections should override
      // corresponding generated sections. 'Merge-two-topics' keeps the first of
      // its arguments, so put authored topics first.
      let topics = concatenate(authored-topics, generated-topics);
      reduce1(merge-two-topics, topics)
   end if
end method;


define method merge-two-topics (a :: <topic>, b :: <topic>)
=> (merged :: <topic>)
   a.fixed-parent := a.fixed-parent | b.fixed-parent;
   a.parent := a.parent | b.parent;

   // Use generated title if possible.
   if (b.generated-topic? & ~b.title.empty?)
      a.title := b.title;
      a.title-source-loc := b.title-source-loc;
   end if;
   
   if (~a.id & b.id)
      a.id := b.id;
      a.id-source-loc := b.id-source-loc;
   end if;

   a.shortdesc := a.shortdesc | b.shortdesc;
   if (a.content.empty?)
      a.content := b.content
   end if;
   a.footnotes := concatenate!(a.footnotes, b.footnotes);
   a.related-links := concatenate!(a.related-links, b.related-links);
   a.relevant-to := concatenate!(a.relevant-to, b.relevant-to);
   a
end method;


define method merge-two-topics (a :: <api-doc>, b :: <api-doc>)
=> (merged :: <api-doc>)
   debug-assert(~a.fully-qualified-name | ~b.fully-qualified-name |
         a.fully-qualified-name = b.fully-qualified-name,
         "Fully qualified names for %= and %= do not match");
   a.existent-api? := a.existent-api? | b.existent-api?;
   a.canonical-namespace := a.canonical-namespace | b.canonical-namespace;

   for (b-titles keyed-by namespace in b.titles-in-namespace)
      let a-titles = element(a.titles-in-namespace, namespace, default: #[]);
      let combined = union(a-titles, b-titles, test: \=);
      a.titles-in-namespace[namespace] := combined;
   end for;

   a.definitions-section := a.definitions-section | b.definitions-section;
   next-method()
end method;


define method merge-two-topics (a :: <unbound-doc>, b :: <api-doc>)
=> (merged :: <api-doc>)
   merge-two-topics(b, a)
end method;


define method merge-two-topics (a :: <library-doc>, b :: <library-doc>)
=> (merged :: <api-doc>)
   a.modules-section := a.modules-section | b.modules-section;
   next-method()
end method;

define method merge-two-topics (a :: <module-doc>, b :: <module-doc>)
=> (merged :: <api-doc>)
   a.bindings-section := a.bindings-section | b.bindings-section;
   next-method()
end method;

define method merge-two-topics (a :: <class-doc>, b :: <class-doc>)
=> (merged :: <api-doc>)
   check-authored-keywords(a, b);
   a.adjectives-section := a.adjectives-section | b.adjectives-section;
   a.keywords-section := a.keywords-section | b.keywords-section;
   a.conds-section := a.conds-section | b.conds-section;
   a.inheritables-section := a.inheritables-section | b.inheritables-section;
   a.supers-section := a.supers-section | b.supers-section;
   a.subs-section := a.subs-section | b.subs-section;
   a.funcs-on-section := a.funcs-on-section | b.funcs-on-section;
   a.funcs-returning-section := a.funcs-returning-section | b.funcs-returning-section;
   next-method()
end method;

define method merge-two-topics (a :: <variable-doc>, b :: <variable-doc>)
=> (merged :: <api-doc>)
   a.adjectives-section := a.adjectives-section | b.adjectives-section;
   a.value-section := a.value-section | b.value-section;
   next-method()
end method;

define method merge-two-topics (a :: <macro-doc>, b :: <macro-doc>)
=> (merged :: <api-doc>)
   a.syntax-section := a.syntax-section | b.syntax-section;
   a.args-section := a.args-section | b.args-section;
   a.vals-section := a.vals-section | b.vals-section;
   next-method()
end method;

define method merge-two-topics (a :: <function-doc>, b :: <function-doc>)
=> (merged :: <api-doc>)
   check-authored-arguments(a, b);
   check-authored-values(a, b);
   a.adjectives-section := a.adjectives-section | b.adjectives-section;
   a.args-section := a.args-section | b.args-section;
   a.vals-section := a.vals-section | b.vals-section;
   a.conds-section := a.conds-section | b.conds-section;
   next-method()
end method;


/// Synopsis: Warn user if known keywords are undocumented.
define method check-authored-keywords (a :: <class-doc>, b :: <class-doc>) => ()
   // TODO: check-authored-keywords
end method;


/// Synopsis: Warn user if known arguments are undocumented.
define method check-authored-arguments (a :: <function-doc>, b :: <function-doc>) => ()
   // TODO: check-authored-arguments
end method;


/// Synopsis: Warn user if known values are undocumented.
define method check-authored-values (a :: <function-doc>, b :: <function-doc>) => ()
   // TODO: check-authored-values
end method;

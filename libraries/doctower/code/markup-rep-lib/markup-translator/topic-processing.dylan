module: markup-translator


/** Synopsis: Topic tokens. **/
define constant <topic-token> =
      type-union(<directive-topic-token>, <titled-topic-token>);
      

/**
Generic Function: make-topic-from-token
---------------------------------------
Synopsis: Generate a topic from a topic token.
**/
define generic make-topic-from-token (token :: <topic-token>)
=> (topic :: <topic>);


define method make-topic-from-token (token :: <directive-topic-token>)
=> (topic :: <topic>)
   let topic-type = token.token-topic-type;
   if (topic-type = #"topic" & dynamic-binding(*catalog-topics*))
      topic-type := #"catalog"
   end if;
   let topic = make(<topic>, source-location: token.token-src-loc,
                    topic-type: topic-type);
   process-tokens(topic, token);
   topic
end method;


define method make-topic-from-token (token :: <titled-topic-token>)
=> (topic :: <con-topic>)
   let topic-type =
         if (dynamic-binding(*catalog-topics*)) #"catalog" else #"topic" end;
   let topic = make(<topic>, source-location: token.token-src-loc,
                    topic-type: topic-type);
   process-tokens(topic, token);
   topic
end method;


//
// Processing general topic content tokens
//


/** Tokens other than those explicitly handled go into 'topic.content'. **/
define method process-tokens
   (topic :: <topic>, token :: <token>)
=> ()
   process-tokens(topic.content, token)
end method;


//
// Processing <directive-topic-token> & <titled-topic-token>
//


define method process-tokens
   (topic :: <topic>, token :: <topic-token>)
=> ()
   process-tokens(topic, token.topic-title);
   process-tokens(topic, token.topic-nickname);
   process-tokens(topic, token.token-content);
end method;


//
// Processing <topic-or-section-title-token>, <topic-directive-title-token>,
// <topic-nickname-token>
//


/**
The title for library or module topics will have "Library" or "Module" added to
the end, but not here; when merging topics, automatically-generated canonical
titles replace user-defined titles and that takes care of "Library" and "Module"
and spaces and casing.
**/
define method process-tokens
   (topic :: <topic>,
    token :: type-union(<topic-or-section-title-token>, <topic-directive-title-token>))
=> ()
   // Determine parent topic by title style.

   let level-styles = dynamic-binding(*topic-level-styles*);
   let level-topics = dynamic-binding(*topic-level-topics*);
   let (level, style) =
         if (instance?(token, <topic-or-section-title-token>))
            let style = token.title-style;
            let level = find-key(level-styles, curry(\=, style),
                                 failure: level-styles.size);
            values(level, style)
         else
            values(0, #f)
         end if;

   if (level > 0)
      topic.fixed-parent := level-topics[level - 1];
   end if;
   level-styles[level] := style;
   level-topics[level] := topic;
   
   // Clear lower levels so user can't make child topic that jumps levels.
   level-styles.size := level + 1;
   level-topics.size := level + 1;
   
   // Process topic content.

   topic.title-source-loc := token.token-src-loc;
   with-dynamic-bindings (*default-quote-specs* = $default-title-quote-specs,
                          *title-markup* = #t)
      process-tokens(topic.title, token.title-content);
   end with-dynamic-bindings;
   check-title(topic);
end method;


define method process-tokens
   (topic :: <topic>, token :: <title-nickname-token>)
=> ()
   topic.id-source-loc := token.token-src-loc;
   topic.id := token.token-text;
   check-id(topic);
end method;
   

//
// Processing <footnote-token>
//


define method process-tokens
   (topic :: <topic>, token :: <footnote-token>)
=> ()
   let footnote = make(<footnote>, source-location: token.token-src-loc);
   footnote.index := token.token-index;
   process-tokens(footnote.content, token.token-content);
   add!(topic.footnotes, footnote);
end method;


//
// Processing <titled-section-token> and <section-directive-token>
//


define method process-tokens
   (topic :: <topic>,
    section-token :: type-union(<titled-section-token>, <section-directive-token>))
=> ()
   let section = make(<section>, source-location: section-token.token-src-loc);
   process-tokens(section, section-token);
   select (section.id by \=)
      ":Declarations" => 
         topic.declarations-section := section;
      ":Modules" =>
         topic.modules-section := section;
      ":Bindings" =>
         topic.bindings-section := section;
      ":Adjectives" =>
         topic.adjectives-section := section;
      ":Inheritables" =>
         topic.inheritables-section := section;
      ":Superclasses" =>
         topic.supers-section := section;
      ":Subclasses" =>
         topic.subs-section := section;
      ":FunctionsOn" =>
         topic.funcs-on-section := section;
      ":FunctionsReturning" =>
         topic.funcs-returning-section := section;
      ":Value" =>
         topic.value-section := section;
      otherwise =>
         add!(topic.content, section);
   end select;
end method;


//
// Processing <paragraph-directive-token> and <division-directive-token>
//


/**
Directive section tokens are added directly to slots of a topic.

Titled section tokens are added to the content of a topic. See
'process-tokens(<topic>, <token>)' and 'process-tokens(<topic-content-seq>,
<titled-section-token>)'.
**/
define method process-tokens
   (topic :: <topic>, section-token :: <directive-section-token>)
=> ()
   check-allowed-sections(section-token, topic);
   select (section-token.directive-type)
      #"synopsis" =>
         check-no-shortdesc(topic);
         topic.shortdesc := make(<paragraph>,
                                 source-location: section-token.token-src-loc);
         process-tokens(topic.shortdesc, section-token.token-content);
      #"keywords", #"conditions", #"arguments", #"values" =>
         // TODO: Ensure user hasn't specified multiple of these, like we do
         // for #"synopsis". Should generalize check-no-shortdesc, maybe combine
         // with check-allowed-sections?
         let (section, setter) = make-directive-section
               (section-token.directive-type, section-token.token-src-loc);
         process-tokens(section, section-token.token-content);
         ensure-parm-list(section);
         setter(section, topic);
   end select;
end method;

// TODO: Process "Slot Accessors" sections specially; they don't get added to
// the topic, but instead result in new topics.


//
// Processing <link-directive-token> and <links-directive-token>
//


define method process-tokens
   (topic :: <topic>, section-token :: <link-directive-token>)
=> ()
   let target = make(<target-placeholder>, link: section-token.link.token-text,
                     source-location: section-token.link.token-src-loc);
   select (section-token.directive-type)
      #"parent" =>
         topic.parent := make(<topic-ref>, target: target,
                              source-location: section-token.token-src-loc);
   end select;
end method;


define method process-tokens
   (topic :: <topic>, section-token :: <links-directive-token>)
=> ()
   let targets =
         map(method (link-token :: <link-word-token>) => (link :: <topic-ref>)
                let loc = link-token.token-src-loc;
                // TODO: If link-target is an URL, make a new URL for it
                // instead of a <target-placeholder>.
                let target = make(<target-placeholder>,
                                  link: link-token.token-text,
                                  source-location: loc);
                make(<topic-ref>, target: target, source-location: loc);
             end method,
             section-token.links);
   select (section-token.directive-type)
      #"relevant-to" =>
         topic.relevant-to := concatenate!(topic.relevant-to, targets);
      #"see-also" =>
         topic.related-links := concatenate!(topic.related-links, targets);
   end select;
end method;


//
// Processing <word-directive-token>
//


define method process-tokens
   (topic :: <topic>, section-token :: <word-directive-token>)
=> ()
   select (section-token.directive-type)
      #"in-library" =>
         unless (instance?(topic, <module-doc>))
            library-specifier-in-non-module-topic
                  (location: section-token.token-src-loc)
         end unless;
      #"in-module" =>
         unless (instance?(topic, <binding-doc>))
            module-specifier-in-non-binding-topic
                  (location: section-token.token-src-loc)
         end unless;
   end select;
   topic.canonical-namespace :=
         section-token.word.token-text.standardize-qualified-name;
   topic.canonical-namespace-source-loc := section-token.token-src-loc;
end method;

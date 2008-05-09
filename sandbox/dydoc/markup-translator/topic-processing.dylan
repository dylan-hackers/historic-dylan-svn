module: markup-translator

/// Synopsis: An umbrella type.
define constant <topic-token> =
      type-union(<topic-directive-token>, <titled-topic-token>);
      
/// [ditto <topic-token>]
define constant <section-token> =
      type-union(<paragraph-directive-token>, <indented-directive-token>);


/// Synopsis: Returns <topic>s from a markup block.
define method process-markup
   (tokens :: <sequence>, context-topic :: false-or(<topic>))
=> (result :: <sequence>)
   let (topics, bare-content) = partition(non-section-topic?, tokens);

   when (~bare-content.empty?)
      // Put bare content into context topic.
      when (~context-topic)
         error(make(<simple-warning>, format-string:
               "Please provide a topic; no implicit topic found for this location."))
      end when;
      process-tokens(context-topic, as(<sequence>, bare-content));
   end when;

   concatenate(if (context-topic) vector(context-topic) else #() end,
               map(make-topic-from-token, topics));
   
   // TODO: Resolve markers and footnotes.
end method;


/// Synopsis: Generate a topic from a topic token.
define generic make-topic-from-token (token :: <topic-token>)
=> (topic :: <topic>);


define method make-topic-from-token (token :: <topic-directive-token>)
=> (topic :: <api-doc>)
   // token.topic-title is a simple string, but topic.title is a <title-seq>.
   // topic.id is specified in automatic topic created by scanning an API.
   let topic = make(<api-doc>, topic-type: token.topic-type);
   add!(topic.title, token.topic-title);
   check-topic-title(topic.title);
   check-topic-id(topic.id);

   process-tokens(topic, token.token-content);
   topic
end method;


define method make-topic-from-token (token :: <titled-topic-token>)
=> (topic :: <con-topic>)
   // TODO: Topic fixed-parent needs to be computed. Where? Add level style to <topic>?
   let topic = make(<con-topic>, id: token.topic-nickname);
   process-tokens(topic.title, token.topic-title);
   check-topic-title(topic.title);
   check-topic-id(topic.id);

   process-tokens(topic, token.token-content);
   topic
end method;


/// Generic Function: process-tokens
///
/// Synopsis: Add a token into an intermediate object, merging if necessary.
///
/// 'Owner' could be the object itself or the value of some slot of the object. We
/// don't call this method on the content slot of a topic, because 'tokens' may
/// include things that affect the topic itself, and the topic isn't reachable
/// from its content slot.
///
/// Arguments:
///   owner - An intermediate object or content sequence.
///   tokens - The token or tokens to process and add to 'owner'.
///
define generic process-tokens
   (owner :: type-union(<interm-element>, <content-seq>, <markup-seq>,
                        <title-seq>, <topic-content-seq>),
    tokens :: type-union(<token>, <sequence>))
=> ();


//
// process-tokens for <topic>s
//

define method process-tokens
   (topic :: <topic>, section-token :: <section-token>)
=> ()
   check-allowed-sections(section-token, topic);
   select (section-token.directive-type)
      #"synopsis" =>
         check-no-shortdesc(topic);
         topic.shortdesc := make(<paragraph>);
         process-tokens(topic.shortdesc, section-token.token-content);
      #"keywords", #"conditions", #"arguments", #"values" =>
         // TODO: Ensure user hasn't specified multiple of these, like we do
         // for #"synopsis". Should generalize check-no-shortdesc.
         let (setter, section-id, section-title) =
               select (section-token.directive-type)
                  #"keywords" =>
                     values(keywords-section-setter, ":Keywords", "Make Keywords");
                  #"conditions" =>
                     values(conds-section-setter, ":Conditions", "Conditions");
                  #"arguments" =>
                     values(args-section-setter, ":Arguments", "Arguments");
                  #"values" =>
                     values(vals-section-setter, ":Values", "Values");
               end select;
         let section = make(<section>, id: section-id);
         process-tokens(section.title, section-title);
         process-tokens(section.content, section-token.token-content);
         setter(section, topic);
      #"note", #"warning" =>
         let note-class = select (section-token.directive-type)
                             #"note" => <note>;
                             #"warning" => <warning-note>;
                          end select;
         let note = make(note-class);
         process-tokens(note.content, section-token.token-content);
         add!(topic.content, note);
   end select;
end method;


define method process-tokens
   (topic :: <topic>, section-token :: <link-directive-token>)
=> ()
   let target = make(<target-placeholder>, link: section-token.link);
   select (section-token.directive-type)
      #"section" => topic.parent := target;
   end select;
end method;


/// Fallback method puts tokens in 'topic.content'.
define method process-tokens
   (topic :: <topic>, token :: <token>)
=> ()
   process-tokens(topic.content, token)
end method;


//
// process-tokens for <topic-content-seq>
//

/// Will only be called for <titled-topic-token>s that are sections; topics
/// will have been separated by 'process-markup' already.
define method process-tokens
   (topic-seq :: <topic-content-seq>, section-token :: <titled-topic-token>)
=> ()
   debug-assert(~non-section-topic?, "Non-section being added to topic");

   let section = make(<section>, id: section-token.topic-nickname);
   process-tokens(section.title, section-token.topic-title);
   check-topic-title(section.title);
   check-topic-id(section.id);

   process-tokens(section.content, section-token.token-content);
   add!(topic-seq, section);
end method;


define method process-tokens
   (topic-seq :: <topic-content-seq>, token :: <footnote-token>)
=> ()
   let footnote = make(<footnote>, index: token.token-index);
   process-tokens(footnote.content, token.token-content);
   add!(topic-seq, footnote);
end method;

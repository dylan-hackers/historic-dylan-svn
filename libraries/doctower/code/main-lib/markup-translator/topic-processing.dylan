module: markup-translator

/** Synopsis: Topic tokens. **/
define constant <topic-token> =
      type-union(<directive-topic-token>, <titled-topic-token>);
      
/** [ditto <section-token>] **/
define constant <directive-section-token> =
      type-union(<paragraph-directive-token>, <division-directive-token>);
      
/**
Synopsis: Section tokens.

<link-directive-token> and <links-directive-token> are not included because they
do not have content.
**/
define constant <section-token> =
      type-union(<directive-section-token>, <titled-section-token>);


/** Synopsis: Returns <topic>s from a markup block. **/
define method topics-from-markup
   (token :: <markup-content-token>, context-topic :: false-or(<topic>))
=> (result :: <sequence>)
   if (~context-topic & ~token.default-topic-content.empty?)
      let content = token.default-topic-content;
      let loc = merge-file-source-locations(content.first, content.last);
      no-context-topic-in-block(loc);
   end if;
   
   let topics = make(<stretchy-vector>);
   if (context-topic)
      process-tokens(context-topic, token.default-topic-content);
      add!(topics, context-topic)
   end if;
   topics := concatenate(topics, map(make-topic-from-token, token.token-topics));
   
   // TODO: Resolve markers and footnotes.
   
   topics
end method;


/**
Generic Function: make-topic-from-token
---------------------------------------
Synopsis: Generate a topic from a topic token.
**/
define generic make-topic-from-token (token :: <topic-token>)
=> (topic :: <topic>);


define method make-topic-from-token (token :: <directive-topic-token>)
=> (topic :: <api-doc>)
   let topic = make(<api-doc>, source-location: token.token-src-loc,
                    topic-type: token.topic-type);
   process-tokens(topic, token);
   topic
end method;


define method make-topic-from-token (token :: <titled-topic-token>)
=> (topic :: <con-topic>)
   let topic = make(<con-topic>, source-location: token.token-src-loc);
   process-tokens(topic, token);
   topic
end method;


/**
Generic Function: process-tokens
--------------------------------
Synopsis: Add a token into an intermediate object, merging if necessary.

'Owner' could be the object itself or the value of some slot of the object. We
don't call this method on the content slot of a topic, because 'tokens' may
include things that affect the topic itself, and the topic isn't reachable
from its content slot.

Arguments:
   owner - An intermediate object or content sequence.
   tokens - The token or tokens to process and add to 'owner'.
**/
define generic process-tokens
   (owner :: type-union(<interm-element>, <content-seq>, <markup-seq>,
                        <title-seq>, <topic-content-seq>),
    tokens :: type-union(<token>, <sequence>, singleton(#f)))
=> ();


/** A sequence of tokens is added to <interm-element>s individually. **/
define method process-tokens
   (owner :: type-union(<interm-element>, <content-seq>, <markup-seq>,
                        <title-seq>, <topic-content-seq>),
    tokens :: <sequence>)
=> ()
   do(curry(process-tokens, owner), tokens)
end method;


/** #f is ignored. **/
define method process-tokens
   (owner :: type-union(<interm-element>, <content-seq>, <markup-seq>,
                        <title-seq>, <topic-content-seq>),
    token == #f)
=> ()
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
// Processing <topic-or-section-title-token>, <directive-topic-title-token>
// <topic-nickname-token>
//


define method process-tokens
   (topic :: <topic>, token :: <topic-or-section-title-token>)
=> ()
   topic.title-source-loc := token.token-src-loc;
   with-dynamic-bindings (*default-quote-specs* = $default-title-quote-specs,
                          *title-markup* = #t)
      process-tokens(topic.title, token.title-content);
   end with-dynamic-bindings;
   check-title(topic);
end method;


define method process-tokens
   (topic :: <topic>, token :: <directive-topic-title-token>)
=> ()
   topic.title-source-loc := token.token-src-loc;
   add!(topic.title, token.title-text);
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
// Processing <titled-section-token>
//


define method process-tokens
   (topic :: <topic>, section-token :: <titled-section-token>)
=> ()
   let section = make(<section>, source-location: section-token.token-src-loc);
   process-tokens(section, section-token);
   add!(topic.content, section);
end method;


//
// Processing <paragraph-directive-token> and <division-directive-token>
//


/**
Directive section tokens are added directly to slots of a topic.
TODO: Should they be added to the normal content instead, with the slots only
used for quick-reference by other code?

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
         let section = make(<section>, source-location: section-token.token-src-loc);
         section.id := section-id;
         section.title := vector(section-title);
         process-tokens(section, section-token.token-content);
         setter(section, topic);
   end select;
end method;


//
// Processing <link-directive-token> and <links-directive-token>
//


define method process-tokens
   (topic :: <topic>, section-token :: <link-directive-token>)
=> ()
   let target = make(<target-placeholder>, link: section-token.link.token-text,
                     source-location: section-token.link.token-src-loc);
   select (section-token.directive-type)
      #"section" =>
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
         topic.see-also := concatenate!(topic.see-also, targets);
   end select;
end method;


//
// Processing sections
//


define method process-tokens
   (section :: <section>, token :: <titled-section-token>)
=> ()
   process-tokens(section, token.section-nickname);
   process-tokens(section, token.section-title);
   process-tokens(section, token.token-content);
end method;


define method process-tokens
   (section :: <section>, token :: <topic-or-section-title-token>)
=> ()
   section.title-source-loc := token.token-src-loc;
   with-dynamic-bindings (*default-quote-specs* = $default-title-quote-specs,
                          *title-markup* = #t)
      process-tokens(section.title, token.title-content);
   end with-dynamic-bindings;
   check-title(section);
end method;


define method process-tokens
   (section :: <section>, token :: <title-nickname-token>)
=> ()
   section.id-source-loc := token.token-src-loc;
   section.id := token.token-text;
   check-id(section);
end method;


/** Tokens other than those explicitly handled go into 'section.content'. **/
define method process-tokens
   (section :: <section>, token :: <token>)
=> ()
   process-tokens(section.content, token)
end method;


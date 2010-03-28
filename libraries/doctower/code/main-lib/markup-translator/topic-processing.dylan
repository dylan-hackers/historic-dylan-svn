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
      type-union(<directive-section-token>, <titled-section-token>,
                 <titled-directive-section-token>);


/** Synopsis: Returns <topic>s from a markup block. **/
define method topics-from-markup
   (token :: <markup-content-token>, context-topic :: false-or(<topic>),
    #key internal :: <boolean> = #f)
=> (result :: <sequence>)
   let topics = make(<stretchy-vector>);
   
   with-dynamic-bindings (*internal-markup* = internal)
      unless (token.default-topic-content.empty?)
         if (context-topic)
            process-tokens(context-topic, token.default-topic-content);
            add!(topics, context-topic)
         else
            let content = token.default-topic-content;
            let loc = merge-file-source-locations
                  (content.first.token-src-loc, content.last.token-src-loc);
            no-context-topic-in-block(location: loc);
         end if;
      end unless;

      topics := concatenate(topics, map(make-topic-from-token, token.token-topics));
   
      // TODO: Resolve markers and footnotes.
      
   end with-dynamic-bindings;
   
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
=> (topic :: <topic>)
   let topic = make(<topic>, source-location: token.token-src-loc,
                    topic-type: token.token-topic-type);
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
   (owner :: type-union(<markup-element>, <content-seq>, <markup-seq>,
                        <title-seq>, <topic-content-seq>),
    tokens :: type-union(<token>, <sequence>, singleton(#f)))
=> ();


/** A sequence of tokens is added to <markup-element>s individually. **/
define method process-tokens
   (owner :: type-union(<markup-element>, <content-seq>, <markup-seq>,
                        <title-seq>, <topic-content-seq>),
    tokens :: <sequence>)
=> ()
   do(curry(process-tokens, owner), tokens)
end method;


/** #f is ignored. **/
define method process-tokens
   (owner :: type-union(<markup-element>, <content-seq>, <markup-seq>,
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


/**
The title for library or module topics will have "Library" or "Module" added to
the end, but not here; when merging topics, automatically-generated canonical
titles replace user-defined titles and that takes care of "Library" and "Module"
and spaces and casing.
**/
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
// Processing <titled-section-token> and <titled-directive-section-token>
//


define method process-tokens
   (topic :: <topic>,
    section-token :: type-union(<titled-section-token>, <titled-directive-section-token>))
=> ()
   let section = make(<section>, source-location: section-token.token-src-loc);
   process-tokens(section, section-token);
   select (section.id by \=)
      ":Definitions" => 
         topic.definitions-section := section;
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
      ":Methods" =>
         topic.methods-section := section;
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
         section.title := title-seq(section-title);
         process-tokens(section, section-token.token-content);
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
// Processing <word-directive-token>
//


// TODO: Signal warning if fully-qualified-name section is used outside of an
// <api-doc>.
define method process-tokens
   (topic :: <api-doc>, section-token :: <word-directive-token>)
=> ()
   select (section-token.directive-type)
      #"fully-qualified-name" =>
         topic.fully-qualified-name :=
               standardize-qualified-name(section-token.word.token-text);
         topic.fully-qualified-name-source-loc := section-token.token-src-loc;
   end select;
end method;


//
// Processing sections
//


define method process-tokens
   (section :: <section>,
    token :: type-union(<titled-section-token>, <titled-directive-section-token>))
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
   (section :: <section>, token :: <directive-section-title-token>)
=> ()
   section.title-source-loc := token.token-src-loc;
   add!(section.title, token.title-text);
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


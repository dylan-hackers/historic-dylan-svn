module: markup-translator
synopsis: These functions take tokens from the markup-parser module and turn
          them into the higher-level representations of this module.

       
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
   block ()
      let (topics, bare-content) = partition(non-section-topic?, tokens);

      // Put bare content into context topic.
      when (~bare-content.empty?)
         when (~context-topic)
            error(make(<simple-warning>, format-string:
                  "Please provide a topic; no implicit topic found for this location."))
         end when;
         process-tokens(context-topic, bare-content);
      end when;

      concatenate(if (context-topic) vector(context-topic) else #() end,
                  map(make-topic-from-token, topics));
   cleanup
      $block-markers.remove-all-keys!
   end block;
end method;


/// Synopsis: Generate a topic from a topic token.
define generic make-topic-from-token (token :: <topic-token>)
=> (topic :: <topic>);


define method make-topic-from-token (token :: <topic-directive-token>)
=> (topic :: <api-doc>)
   let topic = make(<api-doc>, topic-type: token.topic-type);

   // token.topic-title is a simple string, but topic.title is a <title>.
   // Make a <title> for token.topic-title. topic.id is specified in automatic
   // topic created by scanning API.
   topic.title := make(<title>, owner: topic);
   push-last(topic.title.content, token.topic-title);
   topic.id := #f;
   process-tokens(topic, token.token-content);

   check-topic-title(topic.title);
   check-topic-id(topic.id);
   topic
end method;


define method make-topic-from-token (token :: <titled-topic-token>)
=> (topic :: <con-topic>)
   // TODO: Topic fixed-parent needs to be computed. Where? Add level style to <topic>?
   let topic = make(<con-topic>);
   topic.title := make(<title>, owner: topic);
   process-tokens(topic.title, token.topic-title);
   topic.id := token.topic-nickname;
   process-tokens(topic, token.token-content);

   check-topic-title(topic.title);
   check-topic-id(topic.id);
   topic
end method;


/// Synopsis: Add a token into an intermediate object, merging if necessary.
/// Arguments:
///   owner - An intermediate object.
///   token - The token or tokens to process and add to 'owner'.
define generic process-tokens
   (owner :: <interm-element>, tokens :: type-union(<token>, <sequence>))
=> ();


//
// process-tokens for <topic>s
//

// TODO: Remove this? Should all <interm-element>s except <markup> and <title>
// have non-sequence fields? Should <markup> and <title> be sub-classes of
// <deque> in addition to <interm-element>, or should they merely have a <deque>
// slot? I prefer the former.
define method process-tokens
   (elem :: <interm-element>, content :: <sequence>)
=> ()
   do(curry(process-tokens, elem), content);
end method;


/// Will only be called for <titled-topic-token>s that are sections; topics
/// will have been separated out already.
define method process-tokens
   (topic :: <topic>, section-token :: <titled-topic-token>)
=> ()
   debug-assert(~non-section-topic?, "Non-section being added to topic");

   let section = make(<section>, owner: topic);
   section.id := section-token.topic-nickname;
   section.title := make(<title>, owner: section);
   process-tokens(section.title, section-token.topic-title);
   process-tokens(section, section-token.token-content);
   
   check-topic-title(section.title);
   check-topic-id(section.id);
   push-last(topic.content, section);
end method;


define method process-tokens
   (topic :: <topic>, section-token :: <section-token>)
=> ()
   check-allowed-sections(section-token, topic);
   select (section-token.directive-type)
      #"synopsis" =>
         check-no-shortdesc(topic);
         topic.shortdesc = make(<markup>, owner: topic);
         process-tokens(topic.shortdesc, section-token.token-content);
      #"keywords", #"conditions", #"arguments", #"values" =>
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
         let section = make(<section>, owner: topic);
         section.id := section-id;
         section.title := make(<title>, owner: section);
         section.content := make(<markup>, owner: section);
         process-tokens(section.title, section-token.topic-title);
         process-tokens(section.content, section-token.token-content);
         setter(section, topic);
      #"note", #"warning" =>
         let note-class = select (section-token.directive-type)
                             #"note" => <note>;
                             #"warning" => <warning-note>;
                          end select;
         let note = make(note-class, owner: topic);
         note.content := make(<markup>, owner: note);
         process-tokens(note.content, section-token.token-content);
         push-last(topic.content, note);
   end select;
end method;


define method process-tokens
   (topic :: <topic>, para-token :: <paragraph-token>)
=> ()
   let para = make(<paragraph>, owner: topic);
   process-tokens(para, para-token.token-content);
   push-last(topic.content, para);
end method;
   

define method process-tokens
   (topic :: <topic>, section-token :: <link-directive-token>)
=> ()
   let target = make(<target-placeholder>, owner: topic,
                     link: section-token.link);
   select (section-token.directive-type)
      #"section" => topic.parent := target;
   end select;
end method;


//
// process-tokens for <paragraph>s and <title>s
//

define method process-tokens
   (para :: type-union(<paragraph>, <title>), word :: <string>)
=> ()
   if (~para.content.empty?) push-last(para.content, ' ') end;
   push-last (para.content, word);
end method;


define method process-tokens
   (para :: type-union(<paragraph>, <title>), quote :: <quote-token>)
=> ()
   let elements = quote-elements(para, quote, $default-quote-spec);
   if (~instance?(elements, <sequence>))
      elements := vector(elements);
   end if;
   if (~para.content.empty?) push-last(para.content, ' ') end;
   if (quote.prequoted-content) push-last(para.content, quote.prequoted-content) end;
   para.content := concatenate!(para.content, elements);
   if (quote.postquoted-content) push-last(para.content, quote.postquoted-content) end;
end method;


define method process-tokens
   (para :: type-union(<paragraph>, <title>), image-ref :: <image-ref-token>)
=> ()
   push-last(para.content,
             make(<inline-image>, owner: para,
                  image: image-ref.filename, alt-text: image-ref.caption | ""));
end method;


define method process-tokens
   (para :: type-union(<paragraph>, <title>), block-tok :: <bracketed-render-block-token>)
=> ()
   let render-content =
         make(if (block-tok.block-type = #"html") <html-content> else <dita-content> end,
              owner: para, content: block-tok.token-content);
   push-last(para.content, render-content);
end method;


//
// process-tokens for <paragraph>s
//

define method process-tokens
   (para :: <paragraph>, ref :: <marker-ref-token>)
=> ()
   let marker = ensure-marker(ref.index);
   push-last(para.content, make(<xref>, owner: para, target: marker));
end method;


define method process-tokens (para :: <paragraph>, ref :: <synopsis-ref-token>)
=> ()
   push-last(para.content, make(<conref>, owner: para, type: #"shortdesc",
                                target: make(<target-placeholder>, link: ref.link)));
end method;


/// Synopsis: Returns the intermediate elements that make up a quote.
///
/// Discussion: Style changes affect the entire render. Link and monospace
/// affect everything inside typographical quotes. The actual link tag is
/// innermost so its text can be replaced easily by a topic title or an
/// <api-name> or <parameter-name> object (perhaps surrounded by an <xref>).
///
/// From innermost to outermost, the quoted text is wrapped in:
/// link-placeholder, term, code, typographical quotes, bib, b, i, u, term formatting, em
///
define method quote-elements
   (quote-owner :: <interm-element>, quote :: <quote-token>, default-specs :: <table>)
=> (outer-quote-elem :: <interm-element>)
   let specs = check-quote-specifiers(quote, default-specs,
                                      for-title: instance?(quote-owner, <title>));
   let interior =
         if (member?(#"sic", specs))
            concatenate(quote.open-quote, quote.quoted-content, quote.close-quote)
         else
            quote.quoted-content
         end if;
   let link-target = if (instance?(specs.last, <string>)) specs.last else interior end;
   let ownable = #f;    // Not all interiors are ownable; this is outermost ownable.
   let owner = #f;      // Not all outermost can own, since elements have to chain;
                        // this is #f if outermost element can't own.
   let first-term = #t;

   for (spec in choose(rcurry(member?, specs),
         #[ #"qv", #"toc", #"term", #"code", #"q", #"qq", #"bib", #"b", #"i", #"u", #"term", #"em" ]))
      let (new-interior, new-owner) = 
            select (spec)
               #"qv", #"toc" =>
                  let link = make( if (spec = #"toc") <toc-xref> else <xref> end );
                  link.text := interior;
                  link.target := make(<target-placeholder>, owner: link,
                                      link: link-target);
                  values(link, link);
               #"term" =>
                  let term = make( if (first-term) <term> else <term-style> end );
                  term.text := interior;
                  first-term := #f;
                  values(term, term);
               #"code" =>
                  let phrase = make(<code-phrase>);
                  phrase.text := interior;
                  values(phrase, phrase);
               #"q" =>
                  if (~instance?(interior, <sequence>))
                     interior := vector(interior);
                  end if;
                  // left and right single curly quotes, decimal entities
                  let left-quote = vector(make(<entity>, code: #x2018));
                  let right-quote = vector(make(<entity>, code: #x2019));
                  values(concatenate-as(<deque>, left-quote, interior, right-quote), #f);
               #"qq" =>
                  if (~instance?(interior, <sequence>))
                     interior := vector(interior);
                  end if;
                  // left and right double curly quotes, decimal entities
                  let left-quote = vector(make(<entity>, code: #x201C));
                  let right-quote = vector(make(<entity>, code: #x201D));
                  values(concatenate-as(<deque>, left-quote, interior, right-quote), #f);
               #"bib" =>
                  let cite = make(<cite>);
                  cite.text := interior;
                  values(cite, cite);
               #"b" =>
                  let bold = make(<bold>);
                  bold.text := interior;
                  values(bold, bold);
               #"i" =>
                  let ital = make(<italic>);
                  ital.text := interior;
                  values(ital, ital);
               #"u" =>
                  let ul = make(<underline>);
                  ul.text := interior;
                  values(ul, ul);
               #"em" =>
                  let em = make(<emphasis>);
                  em.text := interior;
                  values(em, em);
            end select;
      if (new-owner & ownable) ownable.element-owner := new-owner end;
      ownable := new-owner | ownable;
      interior := new-interior;
   end for;
   if (ownable) ownable.element-owner := owner end;
   interior
end method;

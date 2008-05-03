module: markup-translator
synopsis: Code to integrate sections and intermediate objects into the greater whole.


/// Synopsis: References to markers that have yet to be assigned. Scope limited
/// to a file/comment block.
/// Discussion: This table contains actual <ph-marker> instances, but their
/// owners are not defined. When the marker is placed, it is not made anew, but
/// rather retrieved from this table and its owner set.
define constant $block-markers = make(<object-table>);


/// IDs can have any character except space, "/", "[", "]".
define inline method check-topic-id (id :: <string>) => ()
   when (member?(' ', id) | member?('/', id) | member?('[', id) | member?(']', id))
      error("Topic or section tag cannot have space, slash, open bracket, or "
            "close bracket characters.");
   end when;
   when (id.first = ':')
      error("Topic or section tag cannot have leading colon.")
   end when;
end method;


/// Titles (in string form) cannot have leading colon. I say string form,
/// because that is the one used in links, and this restriction is to avoid
/// ambuiguous links.
define inline method check-topic-title (title :: <title>) => ()
   when (title.stringify-title.first = ':')
      error("Title cannot have leading colon.")
   end when;
end method;


/// Synopsis: There can only be one Synopsis section in a topic. This method
/// is used to check within a single topic; this is also checked across multiple
/// topics when merging [TODO].
define inline method check-no-shortdesc (topic :: <topic>) => ()
   when (slot-initialized?(topic, shortdesc))
      error("Only one synopsis in a topic.")
   end when;
end method;


define method check-quote-specifiers
   (quote :: <quote-token>, default-specs :: <table>, #key for-title) => ()
   let specs = remove-duplicates(
         if (quote.quote-spec.empty?)
            default-specs[quote.open-quote]
         else
            quote.quote-spec
         end if);

   when (member?(#"q", specs) & member?(#"qq", specs))
      error("Can't have both q and qq specifiers.")
   end when;
   
   // Titles cannot have links. This excludes qv, toc. Can have code if we don't
   // link to APIs. In DITA, they cannot have <cite>, but we can fake that.
   when (for-title & (member?(#"qv", specs) | member?(#"toc", specs)))
      error("Titles can't have qv or toc specifiers.")
   end when;

   // A quote can basically be a qv/toc, bib, or term. Former trumps latter.
   when (member?(#"qv", specs) | member?(#"toc", specs))
      specs := remove!(specs, #"bib");
      specs := remove!(specs, #"term");
   end when;
   when (member?(#"bib", specs))
      specs := remove!(specs, #"term");
   end when;
end method;


//
// Sections allowed in topics
//


define method check-allowed-sections (section :: <section-token>, topic :: <topic>)
=> ()
   when (~allowed-markup-section?(section, topic))
      error("Topic cannot have %s sections.", section.directive-type)
   end when;
end method;


/// Synopsis: Determine if a given section directive is valid for a topic.
define generic allowed-markup-section?
   (section :: <section-token>, topic :: <topic>)
=> (okay? :: <boolean>);

define method allowed-markup-section?
   (section :: <section-token>, topic :: <topic>)
=> (okay? :: <boolean>)
   member?(section.directive-type, #[ #"synopsis", #"note", #"warning" ])
end method;

define method allowed-markup-section?
   (section :: <section-token>, topic :: <class-doc>)
=> (okay? :: <boolean>)
   (section.directive-type = #"keywords") | next-method()
end method;

define method allowed-markup-section?
   (section :: <section-token>, topic :: <function-doc>)
=> (okay? :: <boolean>)
   member?(section.directive-type, #[ #"arguments", #"values", #"conditions" ])
         | next-method()
end method;

define method allowed-markup-section?
   (section :: <section-token>, topic :: <macro-doc>)
=> (okay? :: <boolean>)
   member?(section.directive-type, #[ #"arguments", #"values" ]) | next-method()
end method;


//
// Classification of topic tokens into <topic> or <section>
//


define method non-section-topic? (token :: <token>)
=> (non-section-topic? :: <boolean>)
   #f
end method;

define method non-section-topic? (token :: <topic-directive-token>)
=> (non-section-topic? :: <boolean>)
   #t
end method;

define method non-section-topic? (token :: <titled-topic-token>)
=> (non-section-topic? :: <boolean>)
   let style = make(<topic-level-style>, char: token.ascii-line-char,
                    under: token.ascii-underline?, mid: token.ascii-midline?,
                    over: token.ascii-overline?);
   style ~= $section-style
end method;


//
//
//


define method ensure-marker (index :: type-union(<character>, <integer>))
=> (marker :: <ph-marker>)
   element($block-markers, index, default: #f) | 
         ($block-markers[index] := make(<ph-marker>, index: index))
end method;

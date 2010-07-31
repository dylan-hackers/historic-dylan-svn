module: markup-translator
synopsis: Code to integrate sections and intermediate objects into the greater whole.


/// IDs can have any character except space, "/", "[", "]".
define inline method check-id (elem :: type-union(<topic>, <section>)) => ()
   let id = elem.id;
   when (id)
      unless (intersection(" /[]", id).empty?)
         illegal-character-in-id(location: elem.id-source-loc);
      end unless;
      when (id.first = ':' & ~dynamic-binding(*internal-markup*, default: #f))
         leading-colon-in-id(location: elem.id-source-loc);
      end when;
   end when;
end method;


/// Titles (in string form) cannot have leading colon. I say string form,
/// because that is the one used in links, and this restriction is to avoid
/// ambiguous links.
define inline method check-title (elem :: type-union(<topic>, <section>)) => ()
   when (elem.title.stringify-title.first = ':')
      leading-colon-in-title(location: elem.title-source-loc);
   end when;
end method;


/// Synopsis: There can only be one Synopsis section in a topic. This method
/// is used to check within a single topic; this is also checked across multiple
/// topics when merging.
define inline method check-no-shortdesc (topic :: <topic>) => ()
   when (topic.shortdesc)
      duplicate-section-in-topic(location: topic.source-location,
                                 section-type: "synopsis");
   end when;
end method;


define inline method check-allowed-sections
   (section :: <section-token>, topic :: <topic>)
=> ()
   when (~allowed-markup-section?(section, topic))
      illegal-section-in-topic(location: section.token-src-loc,
                               section-type: section.directive-type);
   end when;
end method;


/**
Generic Function: allowed-markup-section?
-----------------------------------------
Synopsis: Determine if a given section directive is valid for a topic.
**/
define generic allowed-markup-section?
   (section :: <section-token>, topic :: <topic>)
=> (okay? :: <boolean>);

define method allowed-markup-section?
   (section :: <directive-section-token>, topic :: <topic>)
=> (okay? :: <boolean>)
   member?(section.directive-type, #[ #"synopsis", #"note", #"warning" ])
end method;

define method allowed-markup-section?
   (section :: <directive-section-token>, topic :: <api-doc>)
=> (okay? :: <boolean>)
   (section.directive-type = #"fully-qualified-name") | next-method()
end method;

// TODO: Add slot accessors section to allowed list.
define method allowed-markup-section?
   (section :: <directive-section-token>, topic :: <class-doc>)
=> (okay? :: <boolean>)
   member?(section.directive-type, #[ #"keywords", #"conditions" ]) | next-method()
end method;

define method allowed-markup-section?
   (section :: <directive-section-token>, topic :: <function-doc>)
=> (okay? :: <boolean>)
   member?(section.directive-type, #[ #"arguments", #"values", #"conditions" ])
         | next-method()
end method;

define method allowed-markup-section?
   (section :: <directive-section-token>, topic :: <macro-doc>)
=> (okay? :: <boolean>)
   member?(section.directive-type, #[ #"arguments", #"values" ]) | next-method()
end method;

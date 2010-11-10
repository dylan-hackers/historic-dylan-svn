module: markup-translator


/**
Synopsis: Section tokens.

<link-directive-token> and <links-directive-token> are not included because they
do not have content.
**/
define constant <section-token> =
      type-union(<directive-section-token>, <titled-section-token>,
                 <section-directive-token>);


/** [ditto <section-token>] **/
define constant <directive-section-token> =
      type-union(<paragraph-directive-token>, <division-directive-token>);
      

/** Syn: Make a blank section of the given type. **/
define method make-directive-section
   (section-type :: <symbol>, location :: <source-location>)
=> (section :: <section>, setter :: <function>)
   let (setter, section-id, section-title) =
         select (section-type)
            #"keywords" =>
               values(keywords-section-setter, ":Keywords", "Make keywords");
            #"conditions" =>
               values(conds-section-setter, ":Conditions", "Conditions");
            #"arguments" =>
               values(args-section-setter, ":Arguments", "Arguments");
            #"values" =>
               values(vals-section-setter, ":Values", "Values");
         end select;
   values(make(<section>, source-location: location,
               id: section-id, title: title-seq(section-title)),
          setter)
end method;


//
// Processing sections
//


define method process-tokens
   (section :: <section>,
    token :: type-union(<titled-section-token>, <section-directive-token>))
=> ()
   process-tokens(section, token.section-nickname);
   process-tokens(section, token.section-title);
   process-tokens(section, token.token-content);
end method;


define method process-tokens
   (section :: <section>,
    token :: type-union(<topic-or-section-title-token>, <section-directive-title-token>))
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


//
// Post-process sections
//


define method ensure-parm-list (section :: <section>) => ()
   if (member?(section.id, #[":Arguments", ":Values", ":Keywords"], test: \=))
      section.content := replace-elements!(section.content,
            rcurry(instance?, <defn-list>),
            method (list-of-parm :: <defn-list>) => (parm-list :: <parm-list>)
               let parm-list-class =
                     select (list-of-parm.object-class)
                        <one-line-defn-list> => <one-line-parm-list>;
                        <many-line-defn-list> => <many-line-parm-list>;
                     end select;
               // TODO: Process parm names into API style except for #-words.
               make(parm-list-class, source-location: list-of-parm.source-location,
                    items: list-of-parm.items);
            end method,
            count: 1);
   end if
end method;

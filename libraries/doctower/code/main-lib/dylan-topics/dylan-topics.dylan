module: dylan-topics


define variable $api-list-filename :: false-or(<file-locator>) = #f;

define constant *definitions* :: <equal-table> = make(<equal-table>);


/**
1. Generate automatically-generated topics for all APIs.
2. Generate manually-authored topic content.
**/
define method topics-from-dylan (api-definitions :: <sequence>)
=> (topics :: <sequence>, catalog-topics :: <sequence>)
   // Make <source-name> quick-reference table.
   for (api-defn :: <definition> in api-definitions)
      for (api-name :: <source-name> in api-defn.aliases)
         *definitions*[api-name] := api-defn
      end for;
   end for;
   
   // Generate topic content for definitions.
   verbose-log("Creating documentation from source code");
   let topics = make(<stretchy-vector>);
   let catalog-topics = make(<stretchy-vector>);
   let definition-topics = make(<stretchy-vector>);
   for (api-defn in api-definitions)
      let (new-topics, new-catalogs) = make-source-topics(api-defn);
      topics := concatenate!(topics, new-topics);
      catalog-topics := concatenate!(catalog-topics, new-catalogs);

      when ($api-list-filename)
         let new-definition-topics
               = choose(conjoin(generated-topic?, rcurry(instance?, <api-doc>)),
                        new-topics);
         unless (new-definition-topics.size = 0)
            definition-topics
                  := add!(definition-topics, pair(api-defn, new-definition-topics))
         end unless
      end when
   end for;
   
   // Clean up.
   remove-all-keys!(*definitions*);
   
   // Generate API list.
   when ($api-list-filename)
      verbose-log("Writing fully-qualified API names to %s", $api-list-filename);
      with-open-file (api-list = $api-list-filename, direction: #"output")
         write(api-list,
            "# Each first line is the fully qualified name of a library, module, or binding.\n"
            "# Each subsequent indented line is an alternative name for the library, module,\n"
            "# or binding as declared or used in Dylan source code.\n\n");
         for (defn-topic in definition-topics)
            for (topic in defn-topic.tail)
               write-line(api-list, topic.fully-qualified-name)
            end for;
            for (alias in defn-topic.head.aliases)
               when (instance?(alias.source-location, <file-source-location>))
                  format(api-list, "\tDeclared as \"%s\" at %s\n", 
                         alias, alias.source-location)
               end when
            end for;
            api-list.new-line
         end for
      end with-open-file
   end when;

   // Generate global catalog topics.
   let global-catalogs = #[ #"all-libraries-topic", #"all-modules-topic" ];
   let no-vars = make(<case-insensitive-string-table>);
   for (catalog-type in global-catalogs)
      let new-topics = topics-from-template(topic-template(catalog-type), #f, no-vars);
      topics := concatenate!(topics, new-topics);
      catalog-topics := concatenate!(catalog-topics, new-topics);
   end for;
   
   values(topics, catalog-topics)
end method;


define generic make-source-topics (definition :: <definition>)
=> (topics :: <sequence>, catalog-topics :: <sequence>);


define method make-authored-topics
   (markup-content-tokens :: <sequence>, context-topic :: false-or(<api-doc>))
=> (topics :: <sequence>)
   let topics-per-token = map(rcurry(topics-from-markup, context-topic), 
                              markup-content-tokens);
   let topics = apply(concatenate, #[], topics-per-token);

   // If several markup tokens attach to the same context topic, the context
   // topic will be duplicated. This is unnecessary, so clear out duplicates.
   topics.remove-duplicates
end method;


//
// Source location for automatically-generated topics
//


define class <generated-source-location> (<source-location>)
end class;

define constant $generated-source-location = make(<generated-source-location>);

define method print-message (o :: <generated-source-location>, s :: <stream>)
=> ()
   write(s, "automatically-generated documentation location")
end method;

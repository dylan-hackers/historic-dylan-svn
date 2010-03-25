module: dylan-topics


define variable $api-list-filename :: false-or(<file-locator>) = #f;

define method equal-hash (source-name :: <library-name>, state :: <hash-state>)
=> (hash :: <integer>, state :: <hash-state>)
   string-hash(source-name.library-name, state)
end method;

define method equal-hash (source-name :: <module-name>, state :: <hash-state>)
=> (hash :: <integer>, state :: <hash-state>)
   values-hash(string-hash, state, source-name.library-name, source-name.module-name)
end method;

define method equal-hash (source-name :: <binding-name>, state :: <hash-state>)
=> (hash :: <integer>, state :: <hash-state>)
   values-hash(string-hash, state,
         source-name.library-name, source-name.module-name, source-name.binding-name)
end method;

define constant *definitions* :: <equal-table> = make(<equal-table>);


/**
1. Generate implicit topics for all APIs.
2. Generate explicit topic content.
**/
define method topics-from-dylan (api-definitions :: <sequence>)
=> (topics :: <sequence>)
   // Make <source-name> quick-reference table.
   for (api-defn :: <definition> in api-definitions)
      for (api-name :: <source-name> in api-defn.aliases)
         *definitions*[api-name] := api-defn
      end for;
   end for;
   
   // Make templates.
   create-topic-templates();

   // Generate topic content for definitions.
   verbose-log("Creating documentation from source code");
   let topics = make(<stretchy-vector>);
   let definition-topics = make(<stretchy-vector>);
   for (api-defn in api-definitions)
      let new-topics = make-source-topics(api-defn);
      topics := concatenate!(topics, new-topics);
      definition-topics := add!(definition-topics, pair(api-defn, new-topics));
   end for;
   
   // Clean up.
   discard-topic-templates();
   remove-all-keys!(*definitions*);
   
   // Generate API list.
   when ($api-list-filename)
      verbose-log("Writing fully-qualified API names to %s", $api-list-filename);
      with-open-file (api-list = $api-list-filename, direction: #"output")
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

   log-object("Topics from source", topics); /**/
   topics
end method;


define generic make-source-topics (definition :: <definition>)
=> (topics :: <sequence>);


define method make-explicit-topics
   (markup-content-tokens :: <sequence>, context-topic :: false-or(<api-doc>))
=> (topics :: <sequence>)
   let topics-per-token = map(rcurry(topics-from-markup, context-topic), 
                              markup-content-tokens);
   apply(concatenate, #[], topics-per-token);
end method;


//
// Source location for automatically-generated topics
//


define class <generated-source-location> (<source-location>)
end class;

define constant $generated-source-location = make(<generated-source-location>);

define method print-message(o :: <generated-source-location>, s :: <stream>)
=> ()
   write(s, "automatically-generated documentation")
end method;

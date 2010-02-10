module: dylan-topics


define variable $api-list-filename :: false-or(<file-locator>) = #f;


/**
1. Generate implicit topics for all APIs.
2. Generate explicit topic content.
**/
define method topics-from-dylan (api-definitions :: <sequence>)
=> (topics :: <sequence>)
   verbose-log("Creating documentation from source code");
   
   // Generate topic content for definitions.
   let libraries = choose(rcurry(instance?, <library>), api-definitions);
   let topics = make(<stretchy-vector>);
   let definition-topics = make(<stretchy-vector>);
   with-dynamic-bindings (*libraries* = libraries)
      for (api-defn in api-definitions)
         let new-topics = make-source-topics(api-defn);
         topics := concatenate!(topics, new-topics);
         definition-topics := add!(definition-topics, pair(api-defn, new-topics));
      end for;
   end with-dynamic-bindings;
   
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
   
   topics
end method;


define generic make-source-topics (definition :: <definition>)
=> (topics :: <sequence>);


define method make-explicit-topics
   (markup-content-tokens :: <sequence>, context-topic :: <api-doc>)
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

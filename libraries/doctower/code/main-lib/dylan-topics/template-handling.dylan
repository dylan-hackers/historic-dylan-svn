module: dylan-topics
synopsis: Operations for template documents.


define method topics-from-template
   (template :: <template>, generated-topic :: <topic>, vars :: <table>)
=> (topics :: <sequence>)
   let generated-body = process-template(template, operations: $template-ops, 
         variables: vars);
   let generated-body-stream = make(<canonical-text-stream>,
         inner-stream: make(<sequence-stream>, contents: generated-body));
   let markup-content = parse-internal-markup(generated-body-stream,
         $generated-source-location);
   topics-from-markup(markup-content, generated-topic, internal: #t)
end method;


define constant $template-ops = table(<case-insensitive-string-table>,
      "definition?" => rcurry(instance?, <defined-namespace>),
      "exports" => template-exports,
      "filename" => source-file,
      "id" => canonical-id,
      "library" => template-library,
      "line" => source-start-line,
      "name" => template-name,
      "size" => size,
      "source" => source-location,
      "unknown-reexports" => template-unknown-reexports,
      );


//
// Sort helpers
//


define method sort-comparison-by-name (a :: <definition>, b :: <definition>)
=> (less-than? :: <boolean>)
   a.canonical-name.local-name.as-lowercase < b.canonical-name.local-name.as-lowercase
end method;


define method sort-comparison-by-name (a :: <source-name>, b :: <source-name>)
=> (less-than? :: <boolean>)
   a.local-name.as-lowercase < b.local-name.as-lowercase
end method;


//
// Names
//


define method template-name (defn :: <definition>) => (name :: <string>)
   defn.canonical-name.local-name.as-titlecase
end method;


define method template-name (name :: <source-name>) => (name :: <string>)
   name.local-name.as-titlecase
end method;


define method template-library (name :: <source-name>) => (library :: <library>)
   let lib-name :: <source-name> = make(<library-name>, library: name.library-name);
   *definitions*[lib-name];
end method;


//
// Exported aliases
//


define method template-exports (mod :: <definition>) => (exports :: <sequence>)
   let exported-aliases = make(<stretchy-vector>);
   for (alias :: <source-name> in mod.aliases)
      let enclosing-namespace = *definitions*[alias.enclosing-name];
      if (member?(alias.local-name, enclosing-namespace.exported-names,
                  test: case-insensitive-equal?))
         exported-aliases := add!(exported-aliases, alias);
      end if;
   end for;
   sort(exported-aliases, test: sort-comparison-by-name)
end method;


//
// Unknown reexports
//


define method template-unknown-reexports (namespace :: <defined-namespace>) 
=> (sources :: <sequence>)
   sort(namespace.unknown-reexport-sources, test: sort-comparison-by-name);
end method;


define method template-unknown-reexports (namespace :: <undefined-namespace>)
=> (sources :: <sequence>)
   #[]
end method;


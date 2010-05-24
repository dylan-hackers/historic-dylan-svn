module: dylan-topics
synopsis: Operations for template documents.


define method topics-from-template
   (template :: <template>, generated-topic, vars :: <table>)
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
      "adjectives" => template-adjectives,
      "code?" => template-code?,
      "default" => template-default,
      "definition?" => rcurry(instance?, <defined-namespace>),
      "exports" => template-exports,
      "filename" => source-file,
      "functions-on-class" => template-funcs-on-class,
      "functions-returning-class" => template-funcs-returning-class,
      "id" => canonical-id,
      "inheritable-getters" => effective-slots,
      "keywords" => effective-init-args,
      "library" => template-library,
      "line" => source-start-line,
      "link?" => template-link?,
      "module" => template-module,
      "name" => template-name,
      "size" => size,
      "scope-name" => definition-qualified-name,
      "source" => source-location,
      "subclasses" => effective-subs,
      "superclasses" => effective-supers,
      "text" => template-text,
      "type" => template-type,
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


define method template-name (init-arg :: <init-arg>) => (name :: <string>)
   concatenate(init-arg.symbol, ":")
end method;


define method template-library (name :: <source-name>) => (library :: <library>)
   let lib-name :: <source-name>
         = make(<library-name>, library: name.library-name);
   *definitions*[lib-name];
end method;


define method template-module (name :: <source-name>) => (module :: <module>)
   let mod-name :: <source-name>
         = make(<module-name>, library: name.library-name, module: name.module-name);
   *definitions*[mod-name];
end method;


//
// Scoping
//


define method template-library (module :: <module>) => (library :: <library>)
   let lib-name :: <library-name> = module.canonical-name.enclosing-name;
   *definitions*[lib-name];
end method;


define method template-module (binding :: <binding>) => (module :: <module>)
   let mod-name :: <module-name> = binding.canonical-name.enclosing-name;
   *definitions*[mod-name];
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
// Adjectives
//


define method template-adjectives
   (binding :: type-union(<class-binding>, <constant-binding>, <variable-binding>))
=> (adjs :: <string>)
   if (binding.valid-binding?)
      let adj-strings = map(curry(as, <string>), binding.explicit-defn.adjectives);
      apply(join, " ", adj-strings)
   else
      ""
   end if
end method;


//
// Text
//


define method template-text (frag :: <fragment>) => (text :: <string>)
   source-text-as-string(frag.source-text, #f)
end method;

module: dylan-topics
synopsis: Entry point and overall control for dylan-topics module.


/// Synopsis: Quick reference for referring to definitions by name.
define constant *definitions* :: <equal-table> = make(<equal-table>);


/**
We only generate API reference topics for exported names. This saves us a fair
amount of time. Any documentation comments associated with excluded APIs are
ignored with a warning.

In addition to concept topics and API reference topics, there are catalog
topics. These are automatically-generated lists of all modules, classes, etc.,
separate from the lists included in each library and module page. They are only
included in the documentation when listed in the table of contents. If included,
they act as parents for other topics.

This function generates catalog topics and any other topics included in source
code doc comments. And, since this function is going through all the APIs
anyway, it also makes the API list file.
**/
define method topics-from-dylan (api-definitions :: <sequence>)
=> (topics :: <sequence>, catalog-topics :: <sequence>)
   // Prepare for generated topic files.
   if (*generated-topics-directory*)
      with-file-error-handlers (default-locator: *generated-topics-directory*)
         ensure-directories-exist(*generated-topics-directory*)
      end with-file-error-handlers
   end if;

   // Make <source-name> quick-reference table.
   for (api-defn :: <definition> in api-definitions)
      for (api-name :: <source-name> in api-defn.aliases)
         *definitions*[api-name] := api-defn
      end for;
   end for;
   
   // Generate topic content for definitions.
   verbose-log("Creating documentation from definitions");
   let topics = make(<stretchy-vector>);
   let catalog-topics = make(<stretchy-vector>);
   let definition-topics = make(<stretchy-vector>);
   for (api-defn in api-definitions)
      if (api-defn.should-have-topic?)
         block ()
            verbose-log("Documenting \"%s\"", api-defn.canonical-name);
            let (new-topics, new-catalogs) = make-source-topics(api-defn);
            topics := concatenate!(topics, new-topics);
            catalog-topics := concatenate!(catalog-topics, new-catalogs);
         exception (restart :: <skip-error-restart>)
         end block
      else
         // Warn about ignored topics.
         let markup-topic-sets = map(token-topics, api-defn.all-markup-tokens);
         let markup-topics = reduce(concatenate!, make(<stretchy-vector>),
                                    markup-topic-sets);
         for (markup-topic in markup-topics)
            doc-comment-on-undocumented-api(location: markup-topic.token-src-loc,
                  api-type: api-defn.api-type-name, api-name: api-defn.canonical-name)
         end for;
         if (instance?(api-defn, <defined-namespace>))
            for (markup-topic in api-defn.file-markup-tokens)
               doc-comment-in-undocumented-file(location: markup-topic.token-src-loc,
                     api-type: api-defn.api-type-name, api-name: api-defn.canonical-name)
            end for;
         end if;
         
         // Always make Dylan and Common-Dylan catalogs available.
         if (instance?(api-defn, <namespace>) & api-defn.provenance = #"predefined")
            block()
               verbose-log("Documenting \"%s\"", api-defn.canonical-name);
               let (new-topics, new-catalogs) = make-source-topics(api-defn);
               catalog-topics := concatenate!(catalog-topics, new-catalogs);
            exception (restart :: <skip-error-restart>)
            end block
         end if;
      end if
   end for;
   
   // Generate API list.
   when (*api-list-file*)
      verbose-log("Writing fully-qualified API names to %s", *api-list-file*);
      with-file-error-handlers (default-locator: *api-list-file*)
         ensure-directories-exist(*api-list-file*);
         with-open-file (api-list = *api-list-file*, direction: #"output")
            write(api-list,
                  "# Each first line is the fully qualified name of a library, module, or binding.\n"
                  "# Each subsequent indented line is an alternative name for the library, module,\n"
                  "# or binding found in parsed Dylan source code.\n\n");
            let sorted-defns = sort(api-definitions,
                  test: method (a :: <definition>, b :: <definition>)
                           a.canonical-name < b.canonical-name
                        end);
            for (api-defn in sorted-defns)
               write-line(api-list, api-defn.definition-qualified-name);
               if (instance?(api-defn, <generic-binding>))
                  for (meth-defn in api-defn.implicit-defns)
                     let params = meth-defn.param-list.req-params;
                     write-line(api-list,
                           definition-qualified-name(api-defn, method-params: params))
                  end for
               end if;
               for (alias in sort(api-defn.aliases))
                  let exp? = alias.exported-name?;
                  when (instance?(alias.source-location, <file-source-location>))
                     format(api-list, "\t%s as \"%s\" at %s\n", 
                           if (exp?) "EXPORTED" else "Declared" end,
                           alias, alias.source-location)
                  end when
               end for;
               api-list.new-line
            end for
         end with-open-file
      end with-file-error-handlers
   end when;

   // Clean up.
   remove-all-keys!(*definitions*);
   
   // Generate global catalog topics.
   let new-topics = topics-from-template
         (#"all-catalog-topics", #f, make(<case-insensitive-string-table>));
   topics := concatenate!(topics, new-topics);
   catalog-topics := concatenate!(catalog-topics, new-topics);
   
   values(topics, catalog-topics)
end method;


/**
Synopsis: Determine whether a definition should be documented.

A definition should be documented if it has a name that should be documented.
The following names should be documented:

* Names of defined libraries.
* Names of modules exported from documented libraries.
* Names of bindings exported from documented modules.
* Names of predefined but valid bindings.
* Names of predefined bindings or modules that are renamed and exported.
**/
define generic should-have-topic? (definition :: <definition>) => (topic? :: <boolean>);

define method should-have-topic? (namespace :: <namespace>) => (topic? :: <boolean>)
   (namespace.provenance ~= #"predefined" & namespace.exported-name?)
         | namespace.predefined-and-renamed?
end method;

define method should-have-topic? (binding :: <binding>) => (topic? :: <boolean>)
   (binding.provenance ~= #"predefined" & binding.exported-name?)
         | (binding.provenance = #"predefined" & binding.valid-binding?)
         | binding.predefined-and-renamed?
end method;


/**
Synopsis: Determines whether a given name is exported by this package.

A name is exported by this package if:

* It is a library defined in this package.
* If it is an exported or reexported module of an exported library.
* If it an exported or reexported binding of an exported or reexported module.
**/
define generic exported-name? (defn :: type-union(<definition>, <source-name>))
=> (exported? :: <boolean>);

define method exported-name? (defn :: <definition>)
=> (exported? :: <boolean>)
   any?(exported-name?, defn.aliases)
end method;

define method exported-name? (name :: <library-name>)
=> (exported? :: <boolean>)
   let defn = element(*definitions*, name, default: #f);
   defn.provenance = #"predefined" | instance?(defn, <defined-namespace>)
end method;

define method exported-name? (name :: type-union(<module-name>, <binding-name>))
=> (exported? :: <boolean>)
   let namespace = name.enclosing-name;
   if (namespace.exported-name?)
      let namespace-defn = element(*definitions*, namespace, default: #f);
      if (namespace-defn)
         member?(name.local-name, namespace-defn.exported-names,
                 test: case-insensitive-equal?)
      end if
   end if
end method;


// TODO: Make a global-name? function that works like exported-name? but also
// lists names that originate from a foreign library. This will include Dylan
// and Common-Dylan, so remove those special cases from exported-name? and
// should-have-topic?(<namespace>). Use this function to generate the exports
// section, but not when generating API lists.


define method predefined-and-renamed? (defn :: <definition>)
=> (pre-and-re? :: <boolean>)
   if (defn.provenance = #"predefined")
      let canonical-local = defn.canonical-name.local-name;
      let local-names = map(local-name, defn.aliases);
      any?(complement(curry(case-insensitive-equal?, canonical-local)), local-names)
   end if
end method;


/**
Arguments:
   definition  - The API representation to generate docs for; a <definition>.
Values:
   topics         - A sequence of <topic> objects.
   catalog-topics - The subset of returned 'topics' that are catalog topics.
**/
define generic make-source-topics (definition :: <definition>)
=> (topics :: <sequence>, catalog-topics :: <sequence>);


define method make-authored-topics
   (markup-content-tokens :: <sequence>, context-topic :: false-or(<api-doc>))
=> (topics :: <sequence>)
   let topics-per-token = map(rcurry(topics-from-markup, context-topic), 
                              markup-content-tokens);
   let topics = reduce(concatenate, make(<stretchy-vector>), topics-per-token);

   // If several markup tokens attach to the same context topic, the context
   // topic will be duplicated. This is unnecessary, so clear out duplicates.
   topics.remove-duplicates!
end method;

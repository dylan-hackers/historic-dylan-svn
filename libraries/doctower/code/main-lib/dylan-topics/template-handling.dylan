module: dylan-topics
synopsis: Operations for template documents.


define constant $topic-templates = #[
   #"all-catalog-topics",
   #"lib-catalog-topics",
   #"mod-catalog-topics",
   #"library-topic",
   #"module-topic",
   #"class-topic",
   #"function-topic",
   #"generic-topic",
   #"method-topic",
   #"constant-topic",
   #"variable-topic",
   #"macro-topic",
   #"unbound-topic"
];

define constant $catalog-templates = #[
   #"all-catalog-topics",
   #"lib-catalog-topics",
   #"mod-catalog-topics"
];


define method topics-from-template
   (template-name :: <symbol>, generated-topic, vars :: <table>)
=> (topics :: <sequence>)
   let catalog? = member?(template-name, $catalog-templates);
   let template = template-by-name(template-name);
   let generated-body = process-template(template, operations: $template-ops, 
         variables: vars);
         
   if (*generated-topics-directory*)
      let base-name :: <string> = 
            if (~vars.empty?)
               debug-assert(vars.size = 1, "More than one variable for template");
               let var-object = vars.any-element;
               select (var-object by instance?)
                  <definition> =>
                     format-to-string("%s_%s", template-name,
                           var-object.definition-qualified-name);
                  <generic-method> =>
                     let binding = var-object.generic-binding;
                     let method-params = var-object.method-defn.param-list.req-params;
                     format-to-string("%s_%s", template-name,
                           definition-qualified-name(binding, method-params: method-params));
               end select
            else
               as(<string>, template-name)
            end if;
      let locator = make(<file-locator>, directory: *generated-topics-directory*,
                         base: base-name, extension: *topic-file-extension*);
      with-open-file (template-output = locator, direction: #"output")
         write(template-output, generated-body)
      end with-open-file;
   end if;

   let generated-body-stream = make(<string-stream>, contents: generated-body);
   let markup-content = parse-internal-markup(generated-body-stream,
         $generated-source-location);
   topics-from-markup(markup-content, generated-topic, internal: #t, catalog: catalog?)
end method;


define constant $template-ops = table(<case-insensitive-string-table>,
      // The comments indicate which other operations can follow each operation
      // or the built-in type returned by each operation.
      
      /*
      "lib" => <library>
            .id .name .scope-name .source .exports .unknown-reexports
      "mod" => <module>
            .id .name .scope-name .source .exports .unknown-reexports
      "class" => <class-binding>
            .id .name .scope-name .source .exports .adjectives
            .functions-on-class .functions-returning-class .inheritable-getters
            .keywords .subclasses .superclasses
      "func" => <function-binding>
            .id .name .scope-name .source .exports .adjectives
            .required-arguments .keyword-arguments .rest-argument
            .all-keys-argument? .takes-keywords? .required-values 
            .rest-value
      "gen" => <generic-binding>
            .id .name .scope-name .source .exports .adjectives
            .required-arguments .keyword-arguments .rest-argument
            .all-keys-argument? .takes-keywords? .required-values 
            .rest-value .methods
      "meth" => <generic-method>
            .id .name .source .adjectives .required-arguments
            .keyword-arguments .rest-argument .all-keys-argument?
            .takes-keywords? .required-values .rest-value
      "const" => <constant-binding>
            .id .name .scope-name .source .exports .adjectives .type .value
      "var" => <variable-binding>
            .id .name .scope-name .source .exports .adjectives .type .value
      "macro" => <macro-binding>
            .id .name .scope-name .source .exports
      */    

      "adjectives" => template-adjectives,                  // <string>
      "all-keys-argument?" => template-all-keys-argument?,  // <boolean>
      "base-name" => template-base-name,                    // <string>
      "default" => template-default,
            // .text .source
      "exports" => template-exports,
            // each .name .library .module
      "filename" => source-file,                            // <string>
      "functions-on-class" => template-functions-on-class,
            // each .id .name .scope-name .source .exports .adjectives
            // .required-arguments .keyword-arguments .rest-argument
            // .all-keys-argument? .takes-keywords? .required-values 
            // .rest-value .methods
      "functions-returning-class" => template-functions-returning-class,
            // each .id .name .scope-name .source .exports .adjectives
            // .required-arguments .keyword-arguments .rest-argument
            // .all-keys-argument? .takes-keywords? .required-values 
            // .rest-value .methods
      "id" => template-id,                                  // false-or(<string>)
      "inheritable-getters" => template-inheritable-getters,
            // each .id .name .scope-name .source .exports .adjectives
            // .required-arguments .keyword-arguments .rest-argument
            // .all-keys-argument? .takes-keywords? .required-values 
            // .rest-value .methods
      "keyword-arguments" => template-keyword-arguments,
            // each .name .source .type .default .singleton
      "keywords" => template-keywords,
            // each .name .source .type .default
      "library" => template-library,
            // .id .name .scope-name .source .exports .unknown-reexports
      "line" => source-start-line,                          // <string>
      "methods" => template-methods,
            // each .id .name .source .adjectives .required-arguments
            // .keyword-arguments .rest-argument .all-keys-argument?
            // .takes-keywords? .required-values .rest-value
      "module" => template-module,
            // .id .name .scope-name .source .exports .unknown-reexports
      "name" => template-name,                              // <string>
      "required-arguments" => template-required-arguments,
            // each .name .source .type .singleton
      "required-values" => template-required-values,
            // each .name .source .type .singleton
      "rest-argument" => template-rest-argument,
            // .name .source
      "rest-value" => template-rest-value,
            // .name .source .type .singleton
      "scope-name" => definition-qualified-name,            // <string>
      "singleton" => template-singleton,
            // .text .source
      "source" => template-source,
            // .filename .line
      "subclasses" => template-subclasses,
            // each .id .name .scope-name .source .exports .adjectives
            // .functions-on-class .functions-returning-class .inheritable-getters
            // .keywords .subclasses .superclasses
      "superclasses" => template-superclasses,
            // each .id .name .scope-name .source .exports .adjectives
            // .functions-on-class .functions-returning-class .inheritable-getters
            // .keywords .subclasses .superclasses
      "takes-keywords?" => template-takes-keywords?,        // <boolean>
      "text" => template-text,                              // <string>
      "type" => template-type,
            // .text .id .source
      "unknown-reexports" => template-unknown-reexports,
            // each .name .library .module            
      "value" => template-value
            // .text .source
      );


//
// Sort helpers
//


define method sort-comparison-by-name (a :: <definition>, b :: <definition>)
=> (less-than? :: <boolean>)
   case-insensitive-less?(a.canonical-name.local-name, b.canonical-name.local-name)
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


define method template-name (meth :: <generic-method>) => (name :: <string>)
   let params = meth.method-defn.param-list.req-params;
   let types = canonical-param-types(params, #f);
   let type-list = types.item-string-list | "";
   format-to-string("%s(%s)", meth.template-base-name, type-list.as-titlecase)
end method;


define method template-name (name :: <source-name>) => (name :: <string>)
   name.local-name.as-titlecase
end method;


//
// Types and fragments
// 


define method template-type (obj :: type-union(<init-arg>, <param>, <value>))
=> (type :: false-or(<type-fragment>))
   obj.type
end method;


define method template-text (frag :: <fragment>) => (text :: <string>)
   source-text-as-string(frag.source-text, #f)
end method;


define method template-text (type :: <type-fragment>) => (text :: <string>)
   as-titlecase(next-method())
end method;


define method template-id (type :: <type-fragment>)
=> (id :: false-or(<string>))
   if (type.simple-name?)
      let source-name = type.source-text.first;
      let definition = element(*definitions*, source-name, default: #f);
      if (definition & definition.should-have-topic?)
         definition.canonical-id
      end if
   end if
end method;


//
// Scoping and ID
//


define method template-id (defn :: <definition>)
=> (id :: false-or(<string>))
   defn.should-have-topic? & defn.canonical-id
end method;


define method template-id (meth :: <generic-method>)
=> (id :: false-or(<string>))
   let method-params = meth.method-defn.param-list.req-params;
   canonical-id(meth.generic-binding, method-params: method-params)
end method;


define method template-library (name :: <source-name>)
=> (library :: false-or(<library>))
   let lib-name :: <source-name>
         = make(<library-name>, library: name.library-name);
   element(*definitions*, lib-name, default: #f)
end method;


define method template-module (name :: <source-name>)
=> (module :: false-or(<module>))
   let mod-name :: <source-name>
         = make(<module-name>, library: name.library-name, module: name.module-name);
   element(*definitions*, mod-name, default: #f)
end method;


//
// Exported aliases and source location
//


define method template-source (api-object :: <api-object>)
=> (loc :: false-or(<file-source-location>))
   instance?(api-object.source-location, <file-source-location>)
         & api-object.source-location
end method;


define method template-source (gen-method :: <generic-method>)
=> (loc :: false-or(<file-source-location>))
   instance?(gen-method.method-defn.source-location, <file-source-location>)
         & gen-method.method-defn.source-location
end method;


define method template-exports (api-defn :: <definition>)
=> (exports :: false-or(<sequence>))
   let exported-aliases = make(<stretchy-vector>);
   for (alias :: <source-name> in api-defn.aliases)
      let enclosing-namespace = *definitions*[alias.enclosing-name];
      if (member?(alias.local-name, enclosing-namespace.exported-names,
                  test: case-insensitive-equal?))
         exported-aliases := add!(exported-aliases, alias);
      end if;
   end for;
   let exports = sort(exported-aliases, test: sort-comparison-by-name);
   ~exports.empty? & exports
end method;


define method template-unknown-reexports (namespace :: <defined-namespace>) 
=> (sources :: false-or(<sequence>))
   let sources = sort(namespace.unknown-reexport-sources, test: sort-comparison-by-name);
   ~sources.empty? & sources
end method;

define method template-unknown-reexports (namespace :: <undefined-namespace>)
=> (sources :: false-or(<sequence>))
   #f
end method;


//
// Adjectives
//


define method template-adjectives (binding :: <binding>) => (adjs :: <string>)
   binding.explicit-defn.template-adjectives
end method;


define method template-adjectives (meth :: <generic-method>) => (adjs :: <string>)
   meth.method-defn.template-adjectives
end method;


define method template-adjectives
   (defn :: type-union(<explicit-class-defn>, <explicit-generic-defn>,
      <implicit-generic-defn>, <explicit-function-defn>, <explicit-constant-defn>,
      <explicit-variable-defn>))
=> (adjs :: <string>)
   let adj-strings = map(curry(as, <string>), defn.adjectives);
   apply(join, " ", adj-strings)
end method;


define method template-adjectives (defn == #f) => (adjs :: <string>)
   ""
end method;


//
// Constants and variables
//


define method template-type
   (const/var :: type-union(<constant-binding>, <variable-binding>))
=> (type :: false-or(<type-fragment>))
   const/var.explicit-defn.type
end method;


define method template-value
   (const/var :: type-union(<constant-binding>, <variable-binding>))
=> (value :: false-or(<computed-constant>))
   const/var.explicit-defn & const/var.explicit-defn.value
end method;


//
// Classes
//


define method template-name (init-arg :: <init-arg>) => (name :: <string>)
   init-arg.symbol
end method;


define method template-default (init-arg :: <init-arg>)
=> (default :: false-or(<code-fragment>))
   init-arg.init-spec
end method;


define method template-keywords (binding :: <class-binding>)
=> (keywords :: false-or(<sequence>))
   ~binding.effective-init-args.empty? & binding.effective-init-args
end method;


define method template-superclasses (binding :: <class-binding>)
=> (superclasses :: false-or(<sequence>))
   ~binding.effective-supers.empty? & binding.effective-supers
end method;


define method template-subclasses (binding :: <class-binding>)
=> (subclasses :: false-or(<sequence>))
   ~binding.effective-subs.empty? & binding.effective-subs
end method;


define method template-functions-on-class (binding :: <class-binding>)
=> (functions :: false-or(<sequence>))
   unless (binding.functions-on-class.empty?)
      sort(binding.functions-on-class, test: sort-comparison-by-name)
   end unless
end method;


define method template-functions-returning-class (binding :: <class-binding>)
=> (functions :: false-or(<sequence>))
   unless (binding.functions-returning-class.empty?)
      sort(binding.functions-returning-class, test: sort-comparison-by-name)
   end unless
end method;


define method template-inheritable-getters (binding :: <class-binding>)
=> (functions :: false-or(<sequence>))
   ~binding.effective-slots.empty? & binding.effective-slots
end method;


//
// Functions
//


define method template-methods (defn :: <generic-binding>)
=> (methods :: false-or(<sequence>))
   let methods = map(curry(make, <generic-method>, generic:, defn, method:),
         defn.implicit-defns);
   ~methods.empty? & methods
end method;


define method template-name (func-arg/val :: type-union(<param>, <value>))
=> (name :: <string>)
   func-arg/val.local-name
end method;


define method template-base-name (meth :: <generic-method>) => (name :: <string>)
   meth.generic-binding.template-name
end method;


//
// Arguments and values
//


define method template-singleton
   (param/value :: type-union(<req-param>, <key-param>, <value>))
=> (expr :: false-or(<computed-constant>))
   instance?(param/value.type, <singleton-type-fragment>)
         & param/value.type.singleton-expr
end method;


//
// Required arguments
//


define method template-required-arguments (func :: <binding>)
=> (args :: false-or(<sequence>))
   func.explicit-defn.template-required-arguments
end method;


define method template-required-arguments (meth :: <generic-method>)
=> (args :: false-or(<sequence>))
   meth.method-defn.template-required-arguments
end method;


define method template-required-arguments
   (defn :: type-union(<explicit-generic-defn>, <implicit-generic-defn>,
      <explicit-function-defn>))
=> (args :: false-or(<sequence>))
   defn.param-list.template-required-arguments
end method;


define method template-required-arguments (defn == #f)
=> (args :: false-or(<sequence>))
   #f
end method;


define method template-required-arguments (params :: <param-list>)
=> (args :: false-or(<sequence>))
   ~params.req-params.empty? & params.req-params
end method;


//
// Keyword arguments
//


define method template-keyword-arguments (func :: <binding>)
=> (args :: false-or(<sequence>))
   func.explicit-defn.template-keyword-arguments
end method;


define method template-keyword-arguments (meth :: <generic-method>)
=> (args :: false-or(<sequence>))
   meth.method-defn.template-keyword-arguments
end method;


define method template-keyword-arguments
   (defn :: type-union(<explicit-generic-defn>, <implicit-generic-defn>,
      <explicit-function-defn>))
=> (args :: false-or(<sequence>))
   defn.param-list.template-keyword-arguments
end method;


define method template-keyword-arguments (defn == #f)
=> (args :: false-or(<sequence>))
   #f
end method;


define method template-keyword-arguments (params :: <key-param-list>)
=> (args :: false-or(<sequence>))
   ~params.key-params.empty? & params.key-params
end method;


define method template-keyword-arguments (params :: <param-list>)
=> (args :: false-or(<sequence>))
   #f
end method;


define method template-takes-keywords? (func :: <binding>)
=> (keywords? :: <boolean>)
   func.explicit-defn.template-takes-keywords?
end method;


define method template-takes-keywords? (meth :: <generic-method>)
=> (keywords? :: <boolean>)
   meth.method-defn.template-takes-keywords?
end method;


define method template-takes-keywords?
   (defn :: type-union(<explicit-generic-defn>, <implicit-generic-defn>,
      <explicit-function-defn>))
=> (keywords? :: <boolean>)
   defn.param-list.template-takes-keywords?
end method;


define method template-takes-keywords? (defn == #f)
=> (keywords? :: <boolean>)
   #f
end method;


define method template-takes-keywords? (params :: <key-param-list>)
=> (keywords? :: <boolean>)
   #t
end method;


define method template-takes-keywords? (params :: <param-list>)
=> (keywords? :: <boolean>)
   #f
end method;


define method template-default (func-arg :: <key-param>)
=> (default :: false-or(<code-fragment>))
   func-arg.expr
end method;


//
// #rest arguments
//


define method template-rest-argument (func :: <binding>)
=> (arg :: false-or(<rest-param>))
   func.explicit-defn.template-rest-argument
end method;


define method template-rest-argument (meth :: <generic-method>)
=> (arg :: false-or(<rest-param>))
   meth.method-defn.template-rest-argument
end method;


define method template-rest-argument
   (defn :: type-union(<explicit-generic-defn>, <implicit-generic-defn>,
      <explicit-function-defn>))
=> (arg :: false-or(<rest-param>))
   defn.param-list.template-rest-argument
end method;


define method template-rest-argument (defn == #f)
=> (arg :: false-or(<rest-param>))
   #f
end method;


define method template-rest-argument
   (params :: type-union(<key-param-list>, <var-param-list>))
=> (arg :: false-or(<rest-param>))
   params.rest-param
end method;


define method template-rest-argument (params :: <param-list>)
=> (arg :: false-or(<rest-param>))
   #f
end method;


//
// #all-keys arguments
//


define method template-all-keys-argument? (func :: <binding>)
=> (all-keys? :: <boolean>)
   func.explicit-defn.template-all-keys-argument?
end method;


define method template-all-keys-argument? (meth :: <generic-method>)
=> (all-keys? :: <boolean>)
   meth.method-defn.template-all-keys-argument?
end method;


define method template-all-keys-argument?
   (defn :: type-union(<explicit-generic-defn>, <implicit-generic-defn>,
      <explicit-function-defn>))
=> (all-keys? :: <boolean>)
   defn.param-list.template-all-keys-argument?
end method;


define method template-all-keys-argument? (defn == #f)
=> (all-keys? :: <boolean>)
   #f
end method;


define method template-all-keys-argument? (params :: <key-param-list>)
=> (all-keys? :: <boolean>)
   params.all-keys?
end method;


define method template-all-keys-argument? (params :: <param-list>)
=> (all-keys? :: <boolean>)
   #f
end method;


//
// Required values
//


define method template-required-values (func :: <binding>)
=> (vals :: false-or(<sequence>))
   func.explicit-defn.template-required-values
end method;


define method template-required-values (meth :: <generic-method>)
=> (vals :: false-or(<sequence>))
   meth.method-defn.template-required-values
end method;


define method template-required-values
   (defn :: type-union(<explicit-generic-defn>, <implicit-generic-defn>,
      <explicit-function-defn>))
=> (vals :: false-or(<sequence>))
   defn.value-list.template-required-values
end method;


define method template-required-values (defn == #f)
=> (vals :: false-or(<sequence>))
   #f
end method;


define method template-required-values (vals :: <value-list>)
=> (vals :: false-or(<sequence>))
   ~vals.req-values.empty? & vals.req-values
end method;


//
// #rest values
//


define method template-rest-value (func :: <binding>)
=> (val :: false-or(<rest-value>))
   func.explicit-defn.template-rest-value
end method;


define method template-rest-value (meth :: <generic-method>)
=> (val :: false-or(<rest-value>))
   meth.method-defn.template-rest-value
end method;


define method template-rest-value
   (defn :: type-union(<explicit-generic-defn>, <implicit-generic-defn>,
      <explicit-function-defn>))
=> (val :: false-or(<rest-value>))
   defn.value-list.rest-value
end method;


define method template-rest-value (defn == #f)
=> (val :: false-or(<rest-value>))
   #f
end method;

module: dylan-topics
synopsis: Operations for template documents.


define method topics-from-template
   (template-name :: <symbol>, generated-topic, vars :: <table>)
=> (topics :: <sequence>)
   let template = topic-template(template-name);
   let generated-body = process-template(template, operations: $template-ops, 
         variables: vars);
   let generated-body-stream = make(<string-stream>, contents: generated-body);
   let markup-content = parse-internal-markup(generated-body-stream,
         $generated-source-location);
   topics-from-markup(markup-content, generated-topic, internal: #t)
end method;


define constant $template-ops = table(<case-insensitive-string-table>,
      "adjectives" => template-adjectives,
      "all-keys-argument?" => template-all-keys-argument?,
      "base-name" => template-base-name,
      "code?" => template-code?,
      "default" => template-default,
      "definition?" => template-definition?,
      "exports" => template-exports,
      "filename" => source-file,
      "functions-on-class" => functions-on-class,
      "functions-returning-class" => functions-returning-class,
      "id" => template-id,
      "inheritable-getters" => effective-slots,
      "keyword-arguments" => template-keyword-arguments,
      "keywords" => effective-init-args,
      "library" => template-library,
      "line" => source-start-line,
      "link?" => template-link?,
      "methods" => template-methods,
      "module" => template-module,
      "name" => template-name,
      "object" => template-object,
      "rest-argument" => template-rest-argument,
      "rest-value" => template-rest-value,
      "required-arguments" => template-required-arguments,
      "required-values" => template-required-values,
      "singleton?" => template-singleton?,
      "size" => size,
      "scope-name" => definition-qualified-name,
      "source" => template-source,
      "subclasses" => effective-subs,
      "superclasses" => effective-supers,
      "takes-keywords?" => template-takes-keywords?,
      "text" => template-text,
      "type" => template-type,
      "unknown-reexports" => template-unknown-reexports,
      "value" => template-value
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


define method template-name (meth :: <generic-method>) => (name :: <string>)
   let params = meth.method-defn.param-list.req-params;
   let types = canonical-param-types(params, #f);
   let type-list = types.item-string-list | "";
   format-to-string("%s(%s)", meth.template-base-name, type-list.as-titlecase)
end method;


define method template-name (name :: <source-name>) => (name :: <string>)
   name.local-name.as-titlecase
end method;


define method template-name (init-arg :: <init-arg>) => (name :: <string>)
   init-arg.symbol
end method;


define method template-name (func-arg/val :: type-union(<param>, <value>))
=> (name :: <string>)
   func-arg/val.local-name
end method;


define method template-base-name (meth :: <generic-method>) => (name :: <string>)
   meth.generic-binding.template-name
end method;


//
// Types
// 


define method template-type (obj :: <object>)
=> (type :: false-or(<type-fragment>))
   obj.type
end method;


define method template-type
   (const/var :: type-union(<constant-binding>, <variable-binding>))
=> (type :: false-or(<type-fragment>))
   const/var.explicit-defn.type
end method;


define method template-code? (type :: false-or(<type-fragment>))
=> (code? :: <boolean>)
   type & ~type.simple-name?
end method;


define method template-link? (type :: false-or(<type-fragment>))
=> (link? :: <boolean>)
   type & type.simple-name?
end method;


//
// Scoping
//


define method template-id (defn :: <definition>) => (id :: <string>)
   defn.canonical-id
end method;


define method template-id (meth :: <generic-method>) => (id :: <string>)
   let method-params = meth.method-defn.param-list.req-params;
   canonical-id(meth.generic-binding, method-params: method-params)
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


define method template-definition? (api-defn :: <definition>)
=> (has-definition? :: <boolean>)
   instance?(api-defn.source-location, <file-source-location>)
end method;


define method template-definition? (gen-method :: <generic-method>)
=> (has-definition? :: <boolean>)
   instance?(gen-method.method-defn.source-location, <file-source-location>)
end method;


define method template-source (api-object :: <api-object>)
=> (loc :: <source-location>)
   api-object.source-location
end method;


define method template-source (gen-method :: <generic-method>)
=> (loc :: <source-location>)
   gen-method.method-defn.source-location
end method;


define method template-exports (api-defn :: <definition>) => (exports :: <sequence>)
   let exported-aliases = make(<stretchy-vector>);
   for (alias :: <source-name> in api-defn.aliases)
      let enclosing-namespace = *definitions*[alias.enclosing-name];
      if (member?(alias.local-name, enclosing-namespace.exported-names,
                  test: case-insensitive-equal?))
         exported-aliases := add!(exported-aliases, alias);
      end if;
   end for;
   sort(exported-aliases, test: sort-comparison-by-name)
end method;


define method template-unknown-reexports (namespace :: <defined-namespace>) 
=> (sources :: <sequence>)
   sort(namespace.unknown-reexport-sources, test: sort-comparison-by-name);
end method;

define method template-unknown-reexports (namespace :: <undefined-namespace>)
=> (sources :: <sequence>)
   #[]
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


define method template-value
   (const/var :: type-union(<constant-binding>, <variable-binding>))
=> (value :: false-or(<computed-constant>))
   const/var.explicit-defn & const/var.explicit-defn.value
end method;


//
// Keyword defaults
//


define method template-default (init-arg :: <init-arg>)
=> (default :: false-or(<code-fragment>))
   init-arg.init-spec
end method;


define method template-default (func-arg :: <key-param>)
=> (default :: false-or(<code-fragment>))
   func-arg.expr
end method;


//
// Methods of generic
//


define method template-methods (defn :: <generic-binding>)
=> (methods :: <sequence>)
   map(curry(make, <generic-method>, generic:, defn, method:), defn.implicit-defns)
end method;


//
// Required arguments
//


define method template-required-arguments (func :: <binding>)
=> (args :: <sequence>)
   func.explicit-defn.template-required-arguments
end method;


define method template-required-arguments (meth :: <generic-method>)
=> (args :: <sequence>)
   meth.method-defn.template-required-arguments
end method;


define method template-required-arguments
   (defn :: type-union(<explicit-generic-defn>, <implicit-generic-defn>,
      <explicit-function-defn>))
=> (args :: <sequence>)
   defn.param-list.template-required-arguments
end method;


define method template-required-arguments (defn == #f)
=> (args :: <sequence>)
   #[]
end method;


define method template-required-arguments (params :: <param-list>)
=> (args :: <sequence>)
   params.req-params
end method;


//
// Keyword arguments
//


define method template-keyword-arguments (func :: <binding>)
=> (args :: <sequence>)
   func.explicit-defn.template-keyword-arguments
end method;


define method template-keyword-arguments (meth :: <generic-method>)
=> (args :: <sequence>)
   meth.method-defn.template-keyword-arguments
end method;


define method template-keyword-arguments
   (defn :: type-union(<explicit-generic-defn>, <implicit-generic-defn>,
      <explicit-function-defn>))
=> (args :: <sequence>)
   defn.param-list.template-keyword-arguments
end method;


define method template-keyword-arguments (defn == #f)
=> (args :: <sequence>)
   #[]
end method;


define method template-keyword-arguments (params :: <key-param-list>)
=> (args :: <sequence>)
   params.key-params
end method;


define method template-keyword-arguments (params :: <param-list>)
=> (args :: <sequence>)
   #[]
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
=> (vals :: <sequence>)
   func.explicit-defn.template-required-values
end method;


define method template-required-values (meth :: <generic-method>)
=> (vals :: <sequence>)
   meth.method-defn.template-required-values
end method;


define method template-required-values
   (defn :: type-union(<explicit-generic-defn>, <implicit-generic-defn>,
      <explicit-function-defn>))
=> (vals :: <sequence>)
   defn.value-list.template-required-values
end method;


define method template-required-values (defn == #f)
=> (vals :: <sequence>)
   #[]
end method;


define method template-required-values (vals :: <value-list>)
=> (vals :: <sequence>)
   vals.req-values
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


//
// Arguments and values
//


define method template-singleton?
   (param/value :: type-union(<req-param>, <key-param>, <value>))
=> (singleton? :: <boolean>)
   instance?(param/value.type, <singleton-type-fragment>)
end method;


define method template-object
   (param/value :: type-union(<req-param>, <key-param>, <value>))
=> (object :: <string>)
   source-text-as-string(param/value.type.singleton-expr.source-text, #f)
end method;


//
// Text
//


define method template-text (frag :: <fragment>) => (text :: <string>)
   source-text-as-string(frag.source-text, #f)
end method;


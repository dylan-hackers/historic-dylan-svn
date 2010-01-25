module: dylan-translator
synopsis: Code to create predefined Dylan and Common-Dylan bindings.


/// === Predefined Dylan and Common-Dylan bindings ===
///
/// We predefine the standard Dylan bindings to reduce memory load and provide
/// good canonical names. Otherwise, each binding name from Dylan or
/// Common-Dylan would stand alone as a <placeholder-binding> in its local
/// namespace without being integrated between namespaces.
///
/// We only predefine the bindings listed in the DRM. We assume they are
/// re-exported from Common-Dylan:Dylan and Common-Dylan:Common-Dylan. We do not
/// include other Common-Dylan bindings because they may vary between compilers,
/// plus there are a lot of them.
///
/// We only include names, not arguments, slots, values, etc. The bindings are
/// therefore incomplete and are not documented. However, if Doctower were run
/// on, say, the Common-Dylan code, they would be filled in appropriately and
/// included in the docs, and though the definition itself would not have a
/// source location, the corresponding <implicit/explicit-defn>s would.


define constant $dylan-library = 
      make(<undefined-library>, provenance: #"predefined",
           local-name: make(<library-name>, library: "dylan"));

define constant $dylan-module =
      make(<undefined-module>, provenance: #"predefined",
           local-name: make(<module-name>, library: "dylan", module: "dylan"));


define method make-predefined-apis (context :: <context>) => ()
   let common-dylan-library = make(<undefined-library>, provenance: #"predefined",
         local-name: make(<library-name>, library: "common-dylan"));

   let common-dylan-module = make(<undefined-module>, provenance: #"predefined",
         local-name: make(<module-name>, library: "common-dylan", 
         module:"common-dylan"));

   context.library-definitions["dylan"] := $dylan-library;
   context.library-definitions["common-dylan"] := common-dylan-library;
   
   $dylan-library.definitions["dylan"] := $dylan-module;
   $dylan-library.exported-names := #("dylan");
   
   common-dylan-library.definitions["common-dylan"] := common-dylan-module;
   common-dylan-library.definitions["dylan"] := $dylan-module;
   common-dylan-library.exported-names := #("common-dylan", "dylan");
   $dylan-module.aliases := add!($dylan-module.aliases,
         make(<module-name>, library: "common-dylan", module: "dylan"));
         
   let modules-for-predef = list($dylan-module, common-dylan-module);
   do(curry(make-predefined-class, modules-for-predef), $predef-classes);
   do(curry(make-predefined-generic, modules-for-predef), $predef-generics);
   do(curry(make-predefined-constant, modules-for-predef), $predef-constants);
   do(curry(make-predefined-macro, modules-for-predef), $predef-macros);
end method;


define method make-predefined-class (modules :: <list>, name :: <string>) => ()
   make-predefined-bindings(modules, name, <class-binding>)
end method;

define method make-predefined-generic (modules :: <list>, name :: <string>) => ()
   make-predefined-bindings(modules, name, <generic-binding>)
end method;

define method make-predefined-constant (modules :: <list>, name :: <string>) => ()
   make-predefined-bindings(modules, name, <constant-binding>)
end method;

define method make-predefined-macro (modules :: <list>, name :: <string>) => ()
   make-predefined-bindings(modules, name, <macro-binding>)
end method;

define method make-predefined-bindings 
   (modules :: <list>, name :: <string>, type :: <class>)
=> ()
   let local-names 
         = map(method (mod :: <module>) => (name :: <binding-name>)
                  make(<binding-name>, binding: name, within: mod.canonical-name)
               end, modules);
   let binding = make(type, provenance: #"predefined", explicit: #f,
                      local-name: local-names.head);
   binding.aliases := concatenate(binding.aliases, local-names.tail);
   for (mod in modules)
      mod.definitions[name] := binding;
      mod.exported-names := add!(mod.exported-names, name);
   end for;
end method;


define constant $predef-classes = #[
"<abort>", "<array>", "<boolean>", "<byte-string>", "<character>", "<class>",
"<collection>", "<complex>", "<condition>", "<deque>", "<double-float>",
"<empty-list>", "<error>", "<explicit-key-collection>", "<extended-float>",
"<float>", "<function>", "<generic-function>", "<integer>", "<list>",
"<method>", "<mutable-collection>", "<mutable-explicit-key-collection>",
"<mutable-sequence>", "<number>", "<object-table>", "<object>", "<pair>",
"<range>", "<rational>", "<real>", "<restart>", "<sealed-object-error>",
"<sequence>", "<serious-condition>", "<simple-error>", "<simple-object-vector>",
"<simple-restart>", "<simple-vector>", "<simple-warning>", "<single-float>",
"<singleton>", "<stretchy-collection>", "<stretchy-vector>", "<string>",
"<symbol>", "<table>", "<type-error>", "<type>", "<unicode-string>", "<vector>",
"<warning>"
];

define constant $predef-generics = #[
"*", "+", "-", "/", "<", "<=", "=", "==", ">", ">=", "^", "~", "~=", "~==",
"abort", "abs", "add", "add!", "add-method", "add-new", "add-new!",
"all-superclasses", "always", "any?", "applicable-method?", "apply", "aref",
"aref-setter", "as", "as-lowercase", "as-lowercase!", "as-uppercase",
"as-uppercase!", "ash", "backward-iteration-protocol", "break", "ceiling",
"ceiling/", "cerror", "check-type", "choose", "choose-by", "complement",
"compose", "concatenate", "concatenate-as", "condition-format-arguments",
"condition-format-string", "conjoin", "copy-sequence", "curry",
"default-handler", "dimension", "dimensions", "direct-subclasses",
"direct-superclasses", "disjoin", "do", "do-handlers", "element",
"element-setter", "empty?", "error", "even?", "every?", "fill!", "find-key",
"find-method", "first", "first-setter", "floor", "floor/",
"forward-iteration-protocol", "function-arguments", "function-return-values",
"function-specializers", "gcd", "generic-function-mandatory-", "keywords",
"generic-function-methods", "head", "head-setter", "identity", "initialize",
"instance?", "integral?", "intersection", "key-sequence", "key-test", "last",
"last-setter", "lcm", "limited", "list", "logand", "logbit?", "logior",
"lognot", "logxor", "make", "map", "map-as", "map-into", "max", "member?",
"merge-hash-codes", "min", "modulo", "negative", "negative?", "object-class",
"object-hash", "odd?", "pair", "pop", "pop-last", "positive?", "push",
"push-last", "range", "rank", "rcurry", "reduce", "reduce1", "remainder",
"remove", "remove!", "remove-duplicates", "remove-duplicates!", "remove-key!",
"remove-method", "replace-elements!", "replace-subsequence!", "restart-query",
"return-allowed?", "return-description", "return-query", "reverse", "reverse!",
"round", "round/", "row-major-index", "second", "second-setter", "shallow-copy",
"signal", "singleton", "size", "size-setter", "slot-initialized?", "sort",
"sort!", "sorted-applicable-methods", "subsequence-position", "subtype?",
"table-protocol", "tail", "tail-setter", "third", "third-setter", "truncate",
"truncate/", "type-error-expected-type", "type-error-value", "type-for-copy",
"type-union", "union", "values", "vector", "zero?"
];

define constant $predef-constants = #[
"$permanent-hash-state"
];

define constant $predef-macros = #[
"class-definer", "constant-definer", "domain-definer", "generic-definer",
"library-definer", "method-definer", "module-definer", "variable-definer",
"begin", "block", "case", "for", "if", "method", "select", "unless", "until",
"while", ":=", "|", "&"
];

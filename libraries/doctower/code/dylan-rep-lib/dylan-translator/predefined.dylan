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
/// We only include names, not arguments, slots, values, etc., and we assume all
/// functions are generics. The bindings are therefore incomplete and are not
/// documented. However, if Doctower were run on, say, the Common-Dylan code,
/// they would be filled in appropriately and included in the docs, and though
/// the definition itself would not have a source location, the corresponding
/// <implicit/explicit-defn>s would.


//
// Source location for predefined bindings
//


define class <predefined-source-location> (<source-location>)
end class;

define constant $predefined-source-location = make(<predefined-source-location>);

define method print-message (o :: <predefined-source-location>, s :: <stream>)
=> ()
   write(s, "predefined API element")
end method;


//
// Predefined namespaces and APIs
//


define constant $dylan-library = 
      make(<undefined-library>, provenance: #"predefined",
           source-location: $predefined-source-location,
           local-name: make(<library-name>, library: "Dylan"));

define constant $dylan-module =
      make(<undefined-module>, provenance: #"predefined",
           source-location: $predefined-source-location,
           local-name: make(<module-name>, library: "Dylan", module: "Dylan"));

define constant $common-dylan-library =
      make(<undefined-library>, provenance: #"predefined",
           source-location: $predefined-source-location,
           local-name: make(<library-name>, library: "Common-Dylan"));

define constant $common-dylan-module =
      make(<undefined-module>, provenance: #"predefined",
           source-location: $predefined-source-location,
           local-name: make(<module-name>, library: "Common-Dylan", 
           module:"Common-Dylan"));


define method make-predefined-apis (context :: <context>) => ()
   context.library-definitions["Dylan"] := $dylan-library;
   context.library-definitions["Common-Dylan"] := $common-dylan-library;
   
   $dylan-library.definitions["Dylan"] := $dylan-module;
   $dylan-library.exported-names := #["Dylan"];
   
   $common-dylan-library.definitions["Common-Dylan"] := $common-dylan-module;
   $common-dylan-library.definitions["Dylan"] := $dylan-module;
   $common-dylan-library.exported-names := #["Common-Dylan", "Dylan"];
   $dylan-module.aliases := add!($dylan-module.aliases,
         make(<module-name>, source-location: $predefined-source-location,
              library: "Common-Dylan", module: "Dylan"));
         
   // Dylan and Common Dylan
   let modules-for-predef = vector($dylan-module, $common-dylan-module);
   do(curry(make-predefined-class, modules-for-predef), $predef-classes);
   do(curry(make-predefined-generic, modules-for-predef), $predef-generics);
   do(curry(make-predefined-constant, modules-for-predef), $predef-constants);
   do(curry(make-predefined-macro, modules-for-predef), $predef-macros);
   
   // Common Dylan only
   let modules-for-predef = vector($common-dylan-module);
   do(curry(make-predefined-class, modules-for-predef), $predef-cd-classes);
   do(curry(make-predefined-generic, modules-for-predef), $predef-cd-generics);
   do(curry(make-predefined-macro, modules-for-predef), $predef-cd-macros);
end method;


define method make-predefined-class (modules :: <sequence>, name :: <string>)
=> ()
   make-predefined-bindings(modules, name, <class-binding>)
end method;

define method make-predefined-generic (modules :: <sequence>, name :: <string>)
=> ()
   make-predefined-bindings(modules, name, <generic-binding>)
end method;

define method make-predefined-constant (modules :: <sequence>, name :: <string>)
=> ()
   make-predefined-bindings(modules, name, <constant-binding>)
end method;

define method make-predefined-macro (modules :: <sequence>, name :: <string>) 
=> ()
   make-predefined-bindings(modules, name, <macro-binding>)
end method;

define method make-predefined-bindings 
   (modules :: <sequence>, name :: <string>, type :: <class>)
=> ()
   let local-names = map-as(<list>,
         method (mod :: <module>) => (name :: <binding-name>)
            make(<binding-name>, binding: name, within: mod.canonical-name,
                 source-location: $predefined-source-location)
         end, modules);
         
   let binding = make(type, provenance: #"predefined", explicit: #f,
                      source-location: $predefined-source-location,
                      local-name: local-names.head);
                      
   binding.aliases := concatenate(binding.aliases, local-names.tail);
   
   for (mod in modules)
      mod.definitions[name] := binding;
      mod.exported-names := add!(mod.exported-names, name);
   end for;
end method;


//
// Dylan
//


define constant $predef-classes = #[
"<Abort>", "<Array>", "<Boolean>", "<Byte-string>", "<Character>", "<Class>",
"<Collection>", "<Complex>", "<Condition>", "<Deque>", "<Double-float>",
"<Empty-list>", "<Error>", "<Explicit-key-collection>", "<Extended-float>",
"<Float>", "<Function>", "<Generic-function>", "<Integer>", "<List>",
"<Method>", "<Mutable-collection>", "<Mutable-explicit-key-collection>",
"<Mutable-sequence>", "<Number>", "<Object-table>", "<Object>", "<Pair>",
"<Range>", "<Rational>", "<Real>", "<Restart>", "<Sealed-object-error>",
"<Sequence>", "<Serious-condition>", "<Simple-error>", "<Simple-object-vector>",
"<Simple-restart>", "<Simple-vector>", "<Simple-warning>", "<Single-float>",
"<Singleton>", "<Stretchy-collection>", "<Stretchy-vector>", "<String>",
"<Symbol>", "<Table>", "<Type-error>", "<Type>", "<Unicode-string>", "<Vector>",
"<Warning>"
];

define constant $predef-generics = #[
"*", "+", "-", "/", "<", "<=", "=", "==", ">", ">=", "^", "~", "~=", "~==",
"Abort", "Abs", "Add", "Add!", "Add-method", "Add-new", "Add-new!",
"All-superclasses", "Always", "Any?", "Applicable-method?", "Apply", "Aref",
"Aref-setter", "As", "As-lowercase", "As-lowercase!", "As-uppercase",
"As-uppercase!", "Ash", "Backward-iteration-protocol", "Break", "Ceiling",
"Ceiling/", "Cerror", "Check-type", "Choose", "Choose-by", "Complement",
"Compose", "Concatenate", "Concatenate-as", "Condition-format-arguments",
"Condition-format-string", "Conjoin", "Copy-sequence", "Curry",
"Default-handler", "Dimension", "Dimensions", "Direct-subclasses",
"Direct-superclasses", "Disjoin", "Do", "Do-handlers", "Element",
"Element-setter", "Empty?", "Error", "Even?", "Every?", "Fill!", "Find-key",
"Find-method", "First", "First-setter", "Floor", "Floor/",
"Forward-iteration-protocol", "Function-arguments", "Function-return-values",
"Function-specializers", "Gcd", "Generic-function-mandatory-keywords",
"Generic-function-methods", "Head", "Head-setter", "Identity", "Initialize",
"Instance?", "Integral?", "Intersection", "Key-sequence", "Key-test", "Last",
"Last-setter", "Lcm", "Limited", "List", "Logand", "Logbit?", "Logior",
"Lognot", "Logxor", "Make", "Map", "Map-as", "Map-into", "Max", "Member?",
"Merge-hash-codes", "Min", "Modulo", "Negative", "Negative?", "Object-class",
"Object-hash", "Odd?", "Pair", "Pop", "Pop-last", "Positive?", "Push",
"Push-last", "Range", "Rank", "Rcurry", "Reduce", "Reduce1", "Remainder",
"Remove", "Remove!", "Remove-duplicates", "Remove-duplicates!", "Remove-key!",
"Remove-method", "Replace-elements!", "Replace-subsequence!", "Restart-query",
"Return-allowed?", "Return-description", "Return-query", "Reverse", "Reverse!",
"Round", "Round/", "Row-major-index", "Second", "Second-setter", "Shallow-copy",
"Signal", "Singleton", "Size", "Size-setter", "Slot-initialized?", "Sort",
"Sort!", "Sorted-applicable-methods", "Subsequence-position", "Subtype?",
"Table-protocol", "Tail", "Tail-setter", "Third", "Third-setter", "Truncate",
"Truncate/", "Type-error-expected-type", "Type-error-value", "Type-for-copy",
"Type-union", "Union", "Values", "Vector", "Zero?"
];

define constant $predef-constants = #[
"$Permanent-hash-state"
];

define constant $predef-macros = #[
"Class-definer", "Constant-definer", "Domain-definer", "Generic-definer",
"Library-definer", "Method-definer", "Module-definer", "Variable-definer",
"Begin", "Block", "Case", "For", "If", "Method", "Select", "Unless", "Until",
"While", "|", "&" // excluding ":=" because it is not a legal binding name
];


//
// Common Dylan
//


define constant $predef-cd-classes = #[
"<Byte-character>", "<Format-string-condition>", "<Simple-condition>",
"<String-table>"
];

define constant $predef-cd-generics = #[
"Concatenate!", "Difference", "False-or", "Find-element", "One-of", "Position",
"Remove-all-keys!", "Subclass"
];

define constant $predef-cd-macros = #[
"Assert", "Debug-assert", "Iterate", "When"
];

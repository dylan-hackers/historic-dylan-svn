module: api-rep
synopsis: Representation of API elements extracted from source code.

//
// Root stuff
//

define abstract class <explicit-api> (<object>)
   slot source-token :: false-or(<token>), required-init-keyword: #"source-token";
end class;

define class <library> (<explicit-api>)
   slot local-name :: <string>;
   constant slot used-libraries = make(<stretchy-vector> /* of <used-library> */);
   constant slot modules = make(<stretchy-vector> /* of <module> */);
   constant slot definitions = make(<stretchy-vector> /* of <definition> */);
end class;


//
// Used libraries
//

define class <used-library> (<object>)
   slot import-name :: <string>, init-keyword: #"import-name";
end class;

define method \= (lib1 :: <used-library>, lib2 :: <used-library>) => (equal? :: <boolean>)
   lib1 == lib2 | case-insensitive-equal?(lib1.import-name, lib2.import-name)
end method;


//
// Modules
//

define abstract class <module> (<explicit-api>)
   slot local-name :: <string>, init-keyword: #"local-name";
   constant slot bindings = make(<stretchy-vector> /* of <binding> */);
end class;

define class <exported-module> (<module>)
end class;

define class <reexported-module> (<module>)
   slot import-name :: <string>;
   slot used-library :: <used-library>;
end class;

define class <imported-module> (<module>)
   slot import-name :: <string>;
   slot used-library :: <used-library>;
end class;

define class <internal-module> (<module>)
end class;

/** For modules original to the current library. **/
define method \= (mod1 :: <module>, mod2 :: <module>) => (equal? :: <boolean>)
   mod1 == mod2 | case-insensitive-equal?(mod1.local-name, mod2.local-name)
end method;

/** For module imported from another library. **/
define method \=
   (mod1 :: type-union(<imported-module>, <reexported-module>),
    mod2 :: type-union(<imported-module>, <reexported-module>))
=> (equal? :: <boolean>)
   mod1 == mod2 | (mod1.used-library = mod2.used-library &
                   case-insensitive-equal?(mod1.import-name, mod2.import-name))
end method;


//
// Bindings
//

define abstract class <binding> (<object>)
   slot local-name :: <string>;
   slot definition :: <definition>;
end class;

define class <exported-binding> (<binding>)
end class;

define class <reexported-binding> (<binding>)
   slot import-name :: false-or(<string>);
   slot module :: false-or(<module>);
end class;

define class <internal-binding> (<binding>)
   slot import-name :: false-or(<string>);
   slot module :: false-or(<module>);
end class;


//
// Definitions
//

define abstract class <definition> (<object>)
   slot identifier :: <object>;
end class;

define class <class-defn> (<definition>)
   slot explicit-defn :: false-or(<explicit-class-defn>);
end class;

define class <generic-defn> (<definition>)
   slot explicit-defn :: false-or(<explicit-generic-defn>);
   constant slot implicit-defns
      = make(<stretchy-vector> /* of <implicit-generic-defn> */);
end class;

define class <function-defn> (<definition>)
   slot explicit-defn :: false-or(<explicit-function-defn>);
end class;

define class <constant-defn> (<definition>)
   slot explicit-defn :: false-or(<explicit-constant-defn>);
end class;

define class <variable-defn> (<definition>)
   slot variable-defn :: false-or(<explicit-variable-defn>);
end class;

define class <macro-defn> (<definition>)
   slot macro-defn :: false-or(<explicit-macro-defn>);
end class;

define abstract class <imp/exp-definition> (<object>)
   slot module :: <module>;
end class;


//
// Classes
//

define class <explicit-class-defn> (<imp/exp-definition>, <explicit-api>)
   constant slot adjs
      = make(<stretchy-vector> /* of #"sealed", #"abstract", etc. */);
   constant slot direct-supers
      = make(<stretchy-vector> /* of <class-defn> */);
   constant slot slots
      = make(<stretchy-vector> /* of <slot> */);
   constant slot init-args
      = make(<stretchy-vector> /* of <init-arg> */);
end class;

define abstract class <slot> (<object>)
   slot getter :: <generic-defn>;
   slot setter :: false-or(<generic-defn>);
   slot type :: <type>;
end class;

define abstract class <initable-slot> (<slot>)
   slot init-arg :: false-or(<init-arg>);
   slot init-spec :: false-or(<init-spec>);
end class;

define class <inherited-slot> (<initable-slot>)
end class;

define class <instance-slot> (<initable-slot>)
end class;

define class <class-slot> (<initable-slot>)
end class;

define class <subclass-slot> (<initable-slot>)
end class;

define class <virtual-slot> (<slot>)
end class;


//
// Initializers
// 

define class <init-arg> (<object>)
   slot symbol :: <string>;
   slot type :: <type>;
   slot inferred-type :: false-or(<type>);
   slot init-spec :: false-or(<init-spec>);
end class;

define abstract class <init-spec> (<object>)
   slot code-fragment :: <code-fragment>;
end class;

define class <init-value> (<init-spec>)
end class;

define class <init-expr> (<init-spec>)
end class;


//
// Generics and functions
//

define abstract class <func/gen-definition> (<imp/exp-definition>)
   constant slot adjs
      = make(<stretchy-vector> /* of #"sealed", #"abstract", etc. */);
   slot parameter-list :: <parameter-list>;
   slot inferred-param-list :: false-or(<parameter-list>);
end class;

define class <explicit-generic-defn> (<func/gen-definition>, <explicit-api>)
   slot vendor-options = make(<stretchy-vector> /* of <vendor-option> */);
   slot sealed-domains = make(<stretchy-vector> /* of <sealed-domain> */);
end class;

define class <implicit-generic-defn> (<func/gen-definition>)
   slot warn-sealed-domain? :: <boolean>;
end class;

define class <explicit-function-defn> (<func/gen-definition>, <explicit-api>)
end class;

define class <vendor-option> (<object>)
   slot symbol :: <string>;
   slot code-fragment :: <code-fragment>;
end class;


//
// Parameter lists
//

define class <parameter-list> (<object>)
   constant slot param-list :: <param-list>;
   constant slot value-list :: <value-list>;
end class;

define abstract class <param-list> (<object>)
   constant slot req-params = make(<stretchy-vector> /* of <req-param> */);
end class;

define class <fixed-param-list> (<param-list>)
end class;

define class <key-param-list> (<param-list>)
   constant slot key-params = make(<stretchy-vector> /* of <key-param> */);
   slot all-keys? :: <boolean>;
   slot rest-param :: <rest-param>;
end class;

define class <var-param-list> (<param-list>)
   slot rest-param :: <rest-param>;
end class;

define class <value-list> (<object>)
   constant slot req-values = make(<stretchy-vector> /* of <req-value> */);
   slot rest-value :: false-or(<rest-value>);
end class;

define abstract class <param> (<object>)
   slot local-name :: false-or(<string>);
end class;

define class <req-param> (<param>)
   slot type :: <type>;
end class;

define class <key-param> (<param>)
   slot symbol :: <string>;
   slot type :: <type>;
   slot expr :: <code-fragment>;
end class;

define class <rest-param> (<param>)
end class;

define abstract class <value> (<object>)
   slot local-name :: false-or(<string>);
   slot type :: <type>;
end class;

define class <req-value> (<value>)
end class;

define class <rest-value> (<value>)
end class;


//
// Constants and variables
//

define abstract class <const/var-defn> (<imp/exp-definition>)
   slot type :: <type>;
   slot inferred-type :: false-or(<type>);
   slot value :: <computed-constant>;
end class;

define class <explicit-constant-defn> (<const/var-defn>, <explicit-api>)
end class;

define class <explicit-variable-defn> (<const/var-defn>, <explicit-api>)
end class;


//
// Macros
//

define abstract class <explicit-macro-defn> (<imp/exp-definition>, <explicit-api>)
   constant slot main-rules = make(<stretchy-vector> /* of <rule> */);
   constant slot aux-rules = make(<stretchy-vector> /* of <aux-rules> */);
end class;

define class <explicit-body-macro-defn> (<explicit-macro-defn>)
end class;

define class <explicit-list-macro-defn> (<explicit-macro-defn>)
end class;

define class <explicit-stmt-macro-defn> (<explicit-macro-defn>)
end class;

define class <explicit-func-macro-defn> (<explicit-macro-defn>)
end class;

define class <aux-rules> (<object>)
   slot symbol :: <string>;
   constant slot rules = make(<stretchy-vector> /* of <rule> */);
end class;

define class <rule> (<object>)
   slot pattern :: <pattern>;
   slot template :: <template>;
end class;


//
// Patterns
//

define class <pattern> (<object>)
   constant slot pattern-lists = make(<stretchy-vector> /* of <pattern-list> */);
end class;

define class <pattern-list> (<object>)
   constant slot pattern-sequences
      = make(<stretchy-vector> /* of <pattern-sequence> */);
   slot property-list-pattern :: false-or(<property-list-pattern>);
end class;

define class <pattern-sequence> (<object>)
   constant slot simple-patterns
      = make(<stretchy-vector> /* of <simple-pattern> */);
end class;

define abstract class <simple-pattern> (<object>)
end class;

define class <name-not-end-token> (<simple-pattern>)
   slot name :: <string>;
end class;

define class <arrow-token> (<simple-pattern>)
end class;

define class <bracketed-pattern> (<simple-pattern>)
   slot opening-character :: <character>;
   slot pattern :: <pattern>;
   slot closing-character :: <character>;
end class;

define abstract class <binding-pattern> (<simple-pattern>)
end class;

define class <variable-pattern> (<binding-pattern>)
   slot name :: <pattern-variable>;
   slot type :: <pattern-variable>;
end class;

define class <assignment-pattern> (<binding-pattern>)
   slot variable :: <pattern-variable>;
   slot value :: <pattern-variable>;
end class;

define class <var-assign-pattern> (<binding-pattern>)
   slot name :: <pattern-variable>;
   slot type :: <pattern-variable>;
   slot value :: <pattern-variable>;
end class;

define abstract class <pattern-variable> (<simple-pattern>)
end class;

define class <constrained-name-patvar> (<pattern-variable>)
   slot name :: <string>;
   slot constraint :: <string>;
   slot template :: false-or(<template>);
end class;

define class <ellipsis-patvar> (<pattern-variable>)
end class;

define abstract class <property-list-pattern> (<object>)
end class;

define class <rest-pattern> (<property-list-pattern>)
   slot patvar :: <pattern-variable>;
end class;

define class <key-pattern> (<property-list-pattern>)
   constant slot patvar-keys = make(<stretchy-vector> /* of <pattern-keyword> */);
   slot patvar-all-keys? :: <boolean>;
end class;

define class <rest-key-pattern> (<property-list-pattern>)
   slot patvar :: <pattern-variable>;
   constant slot patvar-keys = make(<stretchy-vector> /* of <pattern-keyword> */);
   slot patvar-all-keys? :: <boolean>;
end class;

define abstract class <pattern-keyword> (<object>)
   slot name :: <string>;
   slot constraint :: <string>;
   slot template :: false-or(<template>);
end class;

define class <first-pattern-keyword> (<pattern-keyword>)
end class;

define class <every-pattern-keyword> (<pattern-keyword>)
end class;


//
// Templates
//

define class <template> (<object>)
   slot content :: <string>;
end class;


//
// Fragments
//

define class <fragment> (<object>)
   slot content :: <string>;
end class;

define class <computed-constant> (<fragment>)
end class;

define class <code-fragment> (<fragment>)
end class;

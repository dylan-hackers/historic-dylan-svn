module: dylan-rep
synopsis: Representation of API elements extracted from source code.


/**
=================
Module: dylan-rep
=================

This module contains a number of classes representing libraries, modules,
bindings, etc. The relationships between and information contained by these
classes are designed to be a close match to the information included in a
library by a compiler. In theory, a Dylan compiler or other tool could generate
XML documents describing libraries, and Doctower could read these documents and
create representations of API elements using these classes. Doctower could then
use the representations to resolve references to API elements in, say, the Dylan
module.


===================
Missing information
===================

In an actual compiler, all libraries, modules, bindings, etc. are known. If not,
there is a clear error. This is not the case with Doctower. It only has complete
information about libraries that it documents, and it has no information (other
than what it can infer) about other libraries and their modules or bindings.

This has an effect on documentation. Normally, for a given library, we document:

   * All exported and reexported modules
   * All bindings associated with reexported modules and their definitions
   * All exported and reexported bindings within exported modules and their
     definitions
   * All imported bindings of internal or exported modules to which definitions
     are added

In the presence of unknown libraries, this documentation is affected as follows:

   All exported and reexported modules -
      Reexported modules are not known. We can infer the existence of some of
      these modules. Others must necessarily be omitted from the library's
      module list.

   All bindings associated with reexported modules and their definitions -
      Reexported modules are not known, thus their bindings are not known. If we
      have inferred that a given module exists, we can also infer some of its
      bindings; but only the names, not the definitions. We can list where they
      come from so the user can do additional research. Other bindings must
      necessarily be omitted from the reexported modules' bindings lists.

   All exported and reexported bindings within exported modules and their
   definitions -
      Bindings directly or indirectly reexported from a module of an unknown
      library are not known. We can only infer the bindings' names, not their
      definitions. We can list their origins, but other bindings must be
      omitted.

   All imported bindings of internal or exported modules to which definitions
   are added -
      We can infer the existence of these bindings and list the added
      definitions. They can be listed normally.

The documenter can, of course, document any of the omitted modules or bindings
manually.


Specific inferences, failure modes, and representations
=======================================================

"Inferred" libraries, modules, and bindings are created without a corresponding
defining macro of the appropriate type, e.g., a module created without a
module-definer.

--- Libraries ---

Any library mentioned in a library definition's "use" clause can be inferred to
exist. We can know the library name.

Except as described below, we cannot know what modules are exported by these
libraries. A library that reexports modules from an inferred library includes
the inferred library in its 'unknown-reexport-sources' slot.

Inferred libraries do not appear in the documentation's library list.

An inferred library is an instance of <unknown-library>.

--- Modules ---

Any module mentioned in a library definition's "use" clause's "import,"
"rename," "exclude," or "export" option can be inferred to exist. We can know
the library and module name. If the clause's "import" and "export" options are
both "all" and the used library is inferred, the defined library may contain
unnamed modules.

Any module mentioned in a module definition's "use" clause can be inferred to
exist. We cannot know from which library that module is imported, unless the
library definition mentions it or an inferred library contains the module.

Except as described below, we cannot know what bindings are exported by these
modules. A module that reexports bindings from an inferred module includes the
inferred module in its 'unknown-reexport-sources' slot.

Inferred modules appear in the module list of the documented library if needed.
The library from which a module is imported is irrelevant, but can be listed in
in the module documentation if known. Any inferred bindings are also listed.

An inferred module is an instance of <imported-module> where 'used-library' is
an inferred library, or any module contained by an inferred library.
   
--- Bindings ---

Any binding mentioned in a module definition's "use" clause's "import,"
"rename," "exclude," or "export" option can be inferred to exist. We can know
the module and binding name. If the clause's "import" and "export" options are
both "all" and the used module is inferred, the defined module may contain
unnamed bindings.

Additionally, all class, method, or other definitions are added to a binding
that can be inferred to exist, and bindings mentioned in those definitions (such
as a superclass or type) can also be inferred to exist.

Except as described below, we cannot know what the bindings are bound to.

Inferred bindings appear in the bindings list of the corresponding module. The 
binding's documentation page will at least list the defining module, even if no
other information is available.

An inferred binding is an instance of <imported-binding> where 'used-module'
is an inferred module, or any binding contained by an inferred module.

--- Definitions ---

We cannot infer any definitions, but if we have a definition and an inferred
binding by the same name, we can associate the definition with the binding (and,
if necessary, assume the binding is from a "create" clause).

For these definitions, we can give the inferred binding name and the module.

There are no inferred definitions. Inferred bindings are assumed to be from
"create" clauses and thus have a default definition of #f.
**/


define abstract class <named-api-element> (<object>)
   slot local-name :: <string>, required-init-keyword: #"local-name";
end class;


define abstract class <documentable-api-element> (<object>)
   slot comment-tokens = make(<stretchy-vector> /* of <doc-comment-token>*/),
      init-keyword: #"comments";
end class;


//
// Libraries
//

/**
A library definition lists used libraries and modules. Modules may be created
and exported from a library; created but internal to a library; excluded from
import from another library; imported and re-exported from another library;
imported and re-exported under a different name from another library; or
imported but not re-exported from another library. There can be multiple use
clauses for the same library. Modules created by multiple clauses are merged.

Modules fall into these categories:

   Exported    - Module is listed in export clause. Instance of <local-module>.
   
   Internal    - Module is created via "define module" but not mentioned in
                 "define library". Instance of <local-module>.
                 
   Excluded    - Module is listed in use clause exclude option, or not listed in
                 use clause import option. No representation.
                 
   Reexported  - Module is listed in use clause export option. Instance of
                 <imported-module>.
                 
   Renamed     - Module is listed in use clause export option and given a new
                 name with rename option, import option, or prefix option.
                 Instance of <imported-module>.
                 
   Imported    - Module is listed in use clause import option, but not listed
                 in export option. Instance of <imported-module>.
   
   Dylan-User  - Every library has an internal Dylan-User module that can't be
                 exported from the library. We are not including it, but if it
                 were included, it would be an instance of <local-module> with
                 pre-defined bindings.
**/
define abstract class <library>
      (<named-api-element>, <documentable-api-element>, <source-location-mixin>)
   slot modules = make(<stretchy-vector> /* of <module> */);
   slot unknown-reexport-sources = make(<stretchy-vector> /* of <library> */);
end class;

define class <known-library> (<library>)
end class;

define class <unknown-library> (<library>)
end class;

/**
Method: \= (<library>, <library>)
=================================
Two libraries are equal if they refer to the same library.
**/

define method \= (lib1 :: <library>, lib2 :: <library>)
=> (equal? :: <boolean>)
   case
      lib1 == lib2 => #t;
      case-insensitive-equal?(lib1.local-name, lib2.local-name) => #t;
      otherwise => #f;
   end case
end method;


//
// Modules
//

define abstract class <module>
      (<named-api-element>, <documentable-api-element>, <source-location-mixin>)
   slot bindings = make(<stretchy-vector> /* of <binding> */);
   slot exported? :: <boolean> = #f, init-keyword: #"exported";
   slot unknown-reexport-sources = make(<stretchy-vector> /* of <module> */);
end class;

define class <local-module> (<module>)
end class;

/**
If 'used-library' is false, the module is a stray. It is imported from some
library but we do not know which one. This can happen under the following 
circumstances:

 - The module is mentioned in a module definition's use clause, but has no local
   definition in this library. The module must be imported from some other
   library, but is not mentioned in any of the library definition's use clauses.
   
 - Another library imports the module, but the module has no local definition in
   this library. The module must therefore be imported from another library and
   reexported, but the module is not explicitly mentioned in this library's
   definition.
**/
define class <imported-module> (<module>)
   slot import-name :: <string>, required-init-keyword: #"import-name";
   slot used-library :: false-or(<library>), required-init-keyword: #"used-library";
end class;

define method stray? (mod :: <local-module>) => (stray? :: <boolean>)
   #f
end method;

define method stray? (mod :: <imported-module>) => (stray? :: <boolean>)
   mod.used-library.false?
end method;

/**
Method: \= (<module>, <module>)
===============================
Two modules are equal if they are named the same and either both local or both
imported from the same module in the same library. If the module is a stray, its
'import-name', 'used-library', and class are not reliable and are irrelevant for
comparison purposes; the module is assumed to be equal to any another module
with the same 'local-name'.

Function is not applicable when mod1 and mod2 are in different libraries.
**/

define method \= (mod1 :: <module>, mod2 :: <module>)
=> (equal? :: <boolean>)
   case
      ~case-insensitive-equal?(mod1.local-name, mod2.local-name) => #f;
      mod1.stray? | mod2.stray? => #t;
      otherwise => #f;
   end case;
end method;

define method \= (mod1 :: <local-module>, mod2 :: <local-module>)
=> (equal? :: <boolean>)
   case
      mod1 == mod2 => #t;
      case-insensitive-equal?(mod1.local-name, mod2.local-name) => #t;
      otherwise => #f;
   end case
end method;

define method \= (mod1 :: <imported-module>, mod2 :: <imported-module>)
=> (equal? :: <boolean>)
   case
      mod1 == mod2 => #t;
      ~case-insensitive-equal?(mod1.local-name, mod2.local-name) => #f;
      mod1.stray? | mod2.stray? => #t;
      mod1.used-library = mod2.used-library
            & case-insensitive-equal?(mod1.import-name, mod2.import-name) => #t;
      otherwise => #f;
   end case
end method;


//
// Bindings
//

/**
Modules are not shared between libraries, nor are the bindings they contain.
Instead, each library has its own set of modules and bindings. These bindings
may be copied from another library's module, however. If the module is an
imported or reexported module, its bindings are copied verbatim from the library
and module from whence it came. If the module is an internal or exported module,
its bindings come from other (used) modules from the same library as well as any
bindings in the module itself.

Bindings may be defined and exported from a module; defined but internal to a
module; undefined but exported from a module; excluded from import from another
module; imported and re-exported from another module; imported and re-exported
under a different name from another module; or imported but not re-exported from
another module. There can be multiple use clauses for the same module. Bindings
created by multiple clauses are merged.

Bindings fall into these categories:

   Exported    - Binding is listed in export clause and has a definition; also,
                 all bindings in a module imported or reexported from another
                 library are of this type. Instance of <local-binding>.
   
   Internal    - Binding is defined but not mentioned in "define module." These
                 bindings do not "need" [em] to be tracked, as the binding
                 itself and any definitions associated with it are not exported
                 from the module. Any other bindings that mention an internal
                 binding will do so in an expression (i.e. as text).

                 We cannot distinguish an internal binding from an imported or
                 reexported binding that is not specifically listed, therefore
                 it has no representation; otherwise, it would be an instance of
                 <local-binding>.

   Excluded    - Binding is listed in use clause exclude option, or not listed
                 in use clause import option. No representation.
                 
   Reexported  - Binding is listed in use clause export option. Definition may
                 be added to binding. Instance of <imported-binding>.
                 
   Renamed     - Binding is listed in use clause export option and given a new
                 name with rename option, import option, or prefix option.
                 Definition may be added to binding. Instance of
                 <imported-binding>.
                 
   Imported    - Binding is listed in use clause import option, but not listed
                 in export option. Definitions may be added to binding. Instance
                 of <imported-binding>.
                 
   Created     - Binding is listed in create clause and does not have a
                 definition. Instance of <local-binding>.

A class, method, or other definition may be associated with a <local-binding> or
an <imported-binding>. If the definition is for an exported binding, it will be
associated with a <local-binding>. In all other cases, we must associate it with
a stray (until resolved) <imported-binding>.

A binding's owner (if known) is the module that has the <local-binding>.
**/
define abstract class <binding> (<named-api-element>, <source-location-mixin>)
   slot definition :: false-or(<definition>) = #f, init-keyword: #"definition";
   slot exported? :: <boolean> = #f, init-keyword: #"exported";
end class;

define class <local-binding> (<binding>)
end class;

/**
If 'used-module' is false, the binding is a stray. It is imported from some
module but we do not know which one. This can happen under the following 
circumstances:

 - Another module imports the binding, but the binding has no local definition in this
   module. The binding must therefore be imported from another module and
   reexported, but the binding is not explicitly mentioned in this module's
   definition.
**/
define class <imported-binding> (<binding>)
   slot import-name :: <string>, required-init-keyword: #"import-name";
   slot used-module :: false-or(<module>), required-init-keyword: #"used-module";
end class;

define method stray? (mod :: <local-binding>) => (stray? :: <boolean>)
   #f
end method;

define method stray? (mod :: <imported-binding>) => (stray? :: <boolean>)
   mod.used-module.false?
end method;

/**
Method: \= (<binding>, <binding>)
=================================
Two bindings are equal if they are named the same and either both local or both
imported from the same binding in the same module. If the binding is imported
from a stray module, any other module with the same name is assumed to be the
same module. If the binding itself is a stray, any other binding with the same
name is assumed to be the same binding.

Binding equality is not related to the API definition; two bindings may refer to
the same definition but not equal each other, and refer to different definitions
but equal each other.

Function is not applicable when bind1 and bind2 are in different modules.
**/

define method \= (bind1 :: <local-binding>, bind2 :: <local-binding>)
=> (equal? :: <boolean>)
   case
      bind1 == bind2 => #t;
      case-insensitive-equal?(bind1.local-name, bind2.local-name) => #t;
      otherwise => #f;
   end case
end method;

define method \= (bind1 :: <imported-binding>, bind2 :: <imported-binding>)
=> (equal? :: <boolean>)
   case
      bind1 == bind2 => #t;
      ~case-insensitive-equal?(bind1.local-name, bind2.local-name) => #f;
      bind1.stray? | bind2.stray? => #t;
      bind1.used-module = bind2.used-module
            & case-insensitive-equal?(bind1.import-name, bind2.import-name) => #t;
      otherwise => #f;
   end case
end method;

define method \= (bind1 :: <local-binding>, bind2 :: <imported-binding>)
=> (equal? :: <boolean>)
   case
      ~case-insensitive-equal?(bind1.local-name, bind2.local-name) => #f;
      bind2.stray? => #t;
      bind2.used-module.stray? => #t;
      otherwise => #f;
   end case
end method;

define inline method \= (bind1 :: <imported-binding>, bind2 :: <local-binding>)
=> (equal? :: <boolean>)
   bind2 = bind1
end method;


//
// Definitions
//

/**
Bindings are owned by specific modules and libraries, but map to definitions
composed of several explicit and implicit definitions gathered from different
libraries. Since definitions aren't wholly contained by a single library or
module, they either have to be independent of libraries and modules. We provide
this independence by referencing the complete definition in every binding
associated with that definition rather than, for example, isolating a method of
a generic function to the module in which the method was defined.
**/
define abstract class <definition> (<object>)
   virtual slot all-defns :: <sequence>;
end class;

define class <class-defn> (<definition>)
   slot explicit-defn :: <explicit-class-defn>;
end class;

define class <generic-defn> (<definition>)
   slot explicit-defn :: false-or(<explicit-generic-defn>);
   slot implicit-defns = make(<stretchy-vector> /* of <implicit-generic-defn> */);
end class;

define class <function-defn> (<definition>)
   slot explicit-defn :: <explicit-function-defn>;
end class;

define class <constant-defn> (<definition>)
   slot explicit-defn :: <explicit-constant-defn>;
end class;

define class <variable-defn> (<definition>)
   slot explicit-defn :: <explicit-variable-defn>;
end class;

define class <macro-defn> (<definition>)
   slot explicit-defn :: <explicit-macro-defn>;
end class;

define method all-defns (gen :: <generic-defn>) => (defns :: <sequence>)
   let defns = if (gen.explicit-defn) vector(gen.explicit-defn) else #[] end;
   concatenate(defns, gen.implicit-defns)
end method;

define method all-defns (defn :: <definition>) => (defns :: <sequence>)
   vector(defn.explicit-defn)
end method;


//
// Classes
//

/**
--- Slots: ---
adjs           - Adjectives to class. Sequence of <symbol>.
direct-supers  - Sequence of <class-defn> or <type-fragment> comprising
                 superclass list.
slots          - Slots in this class. Excludes superclasses. Sequence of <slot>.
init-args      - Initialization keywords defined by "slot" and "keyword"
                 clauses. Excludes superclasses. Sequence of <init-arg>.
**/
define class <explicit-class-defn> (<documentable-api-element>, <source-location-mixin>)
   slot adjs = make(<stretchy-vector>);
   slot direct-supers = make(<stretchy-vector>);
   slot slots = make(<stretchy-vector>);
   slot init-args = make(<stretchy-vector>);
end class;

define abstract class <slot> (<documentable-api-element>)
   slot getter :: <generic-defn>;
   slot setter :: false-or(<generic-defn>);
   slot type :: <type-fragment>;
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

define class <init-arg> (<documentable-api-element>)
   slot symbol :: <string>;
   slot type :: <type-fragment>;
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

define abstract class <func/gen-definition> (<documentable-api-element>)
   slot adjs = make(<stretchy-vector> /* of #"sealed", #"abstract", etc. */);
   slot parameter-list :: <parameter-list>;
   slot inferred-param-list :: false-or(<parameter-list>);
end class;

define class <explicit-generic-defn> (<func/gen-definition>, <source-location-mixin>)
   slot vendor-options = make(<stretchy-vector> /* of <vendor-option> */);
   slot sealed-domains = make(<stretchy-vector> /* of <sealed-domain> */);
end class;

define class <implicit-generic-defn> (<func/gen-definition>, <source-location-mixin>)
   slot warn-sealed-domain? :: <boolean>;
end class;

define class <explicit-function-defn> (<func/gen-definition>, <source-location-mixin>)
end class;

define class <vendor-option> (<object>)
   slot symbol :: <string>;
   slot code-fragment :: <code-fragment>;
end class;


//
// Parameter lists
//

define class <parameter-list> (<object>)
   slot param-list :: <param-list>;
   slot value-list :: <value-list>;
end class;

define abstract class <param-list> (<object>)
   slot req-params = make(<stretchy-vector> /* of <req-param> */);
end class;

define class <fixed-param-list> (<param-list>)
end class;

define class <key-param-list> (<param-list>)
   slot key-params = make(<stretchy-vector> /* of <key-param> */);
   slot all-keys? :: <boolean>;
   slot rest-param :: <rest-param>;
end class;

define class <var-param-list> (<param-list>)
   slot rest-param :: <rest-param>;
end class;

define class <value-list> (<object>)
   slot req-values = make(<stretchy-vector> /* of <req-value> */);
   slot rest-value :: false-or(<rest-value>);
end class;

define abstract class <param> (<documentable-api-element>)
   slot local-name :: false-or(<string>);
end class;

define class <req-param> (<param>)
   slot type :: <type-fragment>;
end class;

define class <key-param> (<param>)
   slot symbol :: <string>;
   slot type :: <type-fragment>;
   slot expr :: <code-fragment>;
end class;

define class <rest-param> (<param>)
end class;

define abstract class <value> (<documentable-api-element>)
   slot local-name :: false-or(<string>);
   slot type :: <type-fragment>;
end class;

define class <req-value> (<value>)
end class;

define class <rest-value> (<value>)
end class;


//
// Constants and variables
//

define abstract class <const/var-defn> (<documentable-api-element>)
   slot type :: <type-fragment>;
   slot value :: <computed-constant>;
end class;

define class <explicit-constant-defn> (<const/var-defn>, <source-location-mixin>)
end class;

define class <explicit-variable-defn> (<const/var-defn>, <source-location-mixin>)
end class;


//
// Macros
//

define abstract class <explicit-macro-defn>
      (<documentable-api-element>, <source-location-mixin>)
   // slot main-rules = make(<stretchy-vector> /* of <rule> */);
   // slot aux-rules = make(<stretchy-vector> /* of <aux-rules> */);
end class;

define class <explicit-body-macro-defn> (<explicit-macro-defn>)
end class;

define class <explicit-list-macro-defn> (<explicit-macro-defn>)
end class;

define class <explicit-stmt-macro-defn> (<explicit-macro-defn>)
end class;

define class <explicit-func-macro-defn> (<explicit-macro-defn>)
end class;

// (unused)
// define class <aux-rules> (<object>)
//    slot symbol :: <string>;
//    slot rules = make(<stretchy-vector> /* of <rule> */);
// end class;
// 
// define class <rule> (<object>)
//    slot pattern :: <pattern>;
//    slot template :: <template>;
// end class;
// 
// 
// //
// // Patterns
// //
// 
// define class <pattern> (<object>)
//    slot pattern-lists = make(<stretchy-vector> /* of <pattern-list> */);
// end class;
// 
// define class <pattern-list> (<object>)
//    slot pattern-sequences = make(<stretchy-vector> /* of <pattern-sequence> */);
//    slot property-list-pattern :: false-or(<property-list-pattern>);
// end class;
// 
// define class <pattern-sequence> (<object>)
//    slot simple-patterns = make(<stretchy-vector> /* of <simple-pattern> */);
// end class;
// 
// define abstract class <simple-pattern> (<object>)
// end class;
// 
// define class <name-not-end-token> (<simple-pattern>)
//    slot name :: <string>;
// end class;
// 
// define class <arrow-token> (<simple-pattern>)
// end class;
// 
// define class <bracketed-pattern> (<simple-pattern>)
//    slot opening-character :: <character>;
//    slot pattern :: <pattern>;
//    slot closing-character :: <character>;
// end class;
// 
// define abstract class <binding-pattern> (<simple-pattern>)
// end class;
// 
// define class <variable-pattern> (<binding-pattern>)
//    slot name :: <pattern-variable>;
//    slot type :: <pattern-variable>;
// end class;
// 
// define class <assignment-pattern> (<binding-pattern>)
//    slot variable :: <pattern-variable>;
//    slot value :: <pattern-variable>;
// end class;
// 
// define class <var-assign-pattern> (<binding-pattern>)
//    slot name :: <pattern-variable>;
//    slot type :: <pattern-variable>;
//    slot value :: <pattern-variable>;
// end class;
// 
// define abstract class <pattern-variable> (<simple-pattern>)
// end class;
// 
// define class <constrained-name-patvar> (<pattern-variable>)
//    slot name :: <string>;
//    slot constraint :: <string>;
//    slot template :: false-or(<template>);
// end class;
// 
// define class <ellipsis-patvar> (<pattern-variable>)
// end class;
// 
// define abstract class <property-list-pattern> (<object>)
// end class;
// 
// define class <rest-pattern> (<property-list-pattern>)
//    slot patvar :: <pattern-variable>;
// end class;
// 
// define class <key-pattern> (<property-list-pattern>)
//    slot patvar-keys = make(<stretchy-vector> /* of <pattern-keyword> */);
//    slot patvar-all-keys? :: <boolean>;
// end class;
// 
// define class <rest-key-pattern> (<property-list-pattern>)
//    slot patvar :: <pattern-variable>;
//    slot patvar-keys = make(<stretchy-vector> /* of <pattern-keyword> */);
//    slot patvar-all-keys? :: <boolean>;
// end class;
// 
// define abstract class <pattern-keyword> (<object>)
//    slot name :: <string>;
//    slot constraint :: <string>;
//    slot template :: false-or(<template>);
// end class;
// 
// define class <first-pattern-keyword> (<pattern-keyword>)
// end class;
// 
// define class <every-pattern-keyword> (<pattern-keyword>)
// end class;
// 
// 
// //
// // Templates
// //
// 
// define class <template> (<object>)
//    slot content :: <string>;
// end class;


//
// Fragments
//

define class <fragment> (<source-location-mixin>)
   slot source-text :: <sequence> /* of <character> and <source-name> */,
      required-init-keyword: #"text";
end class;

define class <source-name> (<source-location-mixin>)
   slot source-name :: <string>, required-init-keyword: #"name";
end class;

define class <computed-constant> (<fragment>)
end class;

define class <type-fragment> (<computed-constant>)
end class;

define class <code-fragment> (<fragment>)
end class;

define class <name-fragment> (<fragment>)
end class;

define method fragment-names (frag :: <fragment>)
=> (names :: <sequence> /* of <source-name> */)
   choose(rcurry(instance?, <source-name>), frag.source-text)
end method;

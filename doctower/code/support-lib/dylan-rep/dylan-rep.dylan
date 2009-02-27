module: dylan-rep
synopsis: Representation of API elements extracted from source code.


/**
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

Specific inferences and failure modes
=====================================

--- Libraries ---

Any library mentioned in a library definition's "use" clause can be inferred to
exist.

Except as described below, we cannot know what modules are exported by these
libraries.

Inferred libraries do not appear in the documentation's library list. If a
library may export other unknown modules, this is noted in the library's
module list.

--- Modules ---

Any module mentioned in a library definition's "use" clause's "import,"
"rename," "exclude," or "export" option can be inferred to exist. We can know
the library and module name. If the "import" and "export" options are both
"all," there may be other modules available from the library that we cannot know
about.

Any module mentioned in a module definitions "use" clause can be inferred to
exist. We cannot know from which library that module is imported.

Except as described below, we cannot know what bindings are exported by these
modules.

Inferred modules appear in the module list of the documented library if needed.
The library from which a module is imported is irrelevant, but can be listed in
in the module documentation if known. Any inferred bindings are also listed. If
the module may export other unknown bindings, this is noted in the module's
bindings list.

--- Bindings ---

Any binding mentioned in a module definition's "use" clause's "import,"
"rename," "exclude," or "export" option can be inferred to exist. We can know
the module and binding name. If the "import" and "export" options are both
"all," there may be other bindings available from the module that we cannot know
about.

Except as described below, we cannot know what the bindings are bound to.

Inferred bindings appear in the bindings list of the corresponding module. The 
binding's documentation page will at least list the defining module, even if no
other information is available.

--- Definitions ---

We cannot infer any definitions, but if we have a definition and an inferred
binding by the same name, we can associate the definition with the binding (and,
if necessary, assume the binding is from a "create" clause).

For these definitions, we can give the inferred binding name and the module.

Representation
==============

Libraries -
   An inferred library is an instance of <unknown-library>. All libraries have
   an 'unknown-reexport-sources' slot that lists inferred, used libraries from
   which unlisted modules may be reexported. If a library instance was created
   simply to hold a module inferred to exist from a module definition's "use"
   clause, the library instance's 'local-name' will be #f.

Modules -
   An inferred module is an instance of <imported-module> where 'used-library'
   is an inferred library. All modules have an 'unknown-reexport-sources' slot
   that lists inferred, used modules from which unlisted bindings may be
   reexported.

Bindings -
   An inferred binding is an instance of <imported-binding> where 'used-module'
   is an inferred module.

Definitions -
   There are no inferred definitions. Inferred bindings are assumed to be from
   "create" clauses and thus have a default definition of #f.
**/


//
// Libraries
//

/**
A library definition lists used libraries and modules. Modules may be created
and exported from the current library; created but internal to the current
library; excluded from import from another library; imported and re-exported
from another library; imported and re-exported under a different name from
another library; or imported but not re-exported from another library. There can
be multiple use clauses for the same library.

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
define abstract class <library> (<source-location-mixin>)
   slot local-name :: false-or(<string>), required-init-keyword: #"local-name";
   slot modules = make(<stretchy-vector> /* of <module> */);
   slot unknown-reexport-sources = make(<stretchy-vector> /* of <library> */);
   virtual slot anonymous? :: <boolean>;
end class;

define class <known-library> (<library>)
   slot used-libraries = make(<stretchy-vector> /* of <library> */);
   slot definitions = make(<stretchy-vector> /* of <definition> */);
end class;

define class <unknown-library> (<library>)
end class;

define method anonymous? (lib :: <library>) => (anon? :: <boolean>)
   ~lib.local-name
end method;

/**
Method: \= (<library>, <library>)
=================================
Two libraries are equal if they refer to the same library. If a library is
anonymous, it is assumed that it could be equal to the other.
**/

define method \= (lib1 :: <library>, lib2 :: <library>)
=> (equal? :: <boolean>)
   case
      lib1 == lib2 => #t;
      lib1.anonymous? | lib2.anonymous? => #t;
      case-insensitive-equal?(lib1.local-name, lib2.local-name) => #t;
      otherwise => #f;
   end case
end method;


//
// Modules
//

define abstract class <module> (<source-location-mixin>)
   slot local-name :: <string>, required-init-keyword: #"local-name";
   slot bindings = make(<stretchy-vector> /* of <binding> */);
   slot exported? :: <boolean> = #f, init-keyword: #"exported";
   slot unknown-reexport-sources = make(<stretchy-vector> /* of <module> */);
end class;

define class <local-module> (<module>)
end class;

define class <imported-module> (<module>)
   slot import-name :: <string>, required-init-keyword: #"import-name";
   slot used-library :: <library>, required-init-keyword: #"used-library";
end class;

/**
Method: \= (<module>, <module>)
===============================
Two modules are equal if they refer to the same module in the same library. If
one of the modules is imported from an anonymous library, that module's
import-name may not be accurate and is irrelevant for comparison purposes.
**/

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
      mod1.used-library.anonymous? | mod2.used-library.anonymous? => #t;
      mod1.used-library = mod2.used-library
            & case-insensitive-equal?(mod1.import-name, mod2.import-name) => #t;
      otherwise => #f;
   end case
end method;


//
// Bindings
//

define abstract class <binding> (<source-location-mixin>)
   slot local-name :: <string>, required-init-keyword: #"local-name";
   slot definition :: false-or(<definition>) = #f, init-keyword: #"definition";
   slot exported? :: <boolean> = #f, init-keyword: #"exported";
end class;

define class <local-binding> (<binding>)
end class;

define class <imported-binding> (<binding>)
   slot import-name :: <string>, required-init-keyword: #"import-name";
   slot used-module :: <module>, required-init-keyword: #"used-module";
end class;


//
// Definitions
//

define abstract class <definition> (<object>)
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

// TODO: This is possibly not a useful class.
define abstract class <imp/exp-definition> (<object>)
   slot module :: <module>;
end class;


//
// Classes
//

define class <explicit-class-defn> (<imp/exp-definition>, <source-location-mixin>)
   slot adjs = make(<stretchy-vector> /* of #"sealed", #"abstract", etc. */);
   slot direct-supers = make(<stretchy-vector> /* of <class-defn> */);
   slot slots = make(<stretchy-vector> /* of <slot> */);
   slot init-args = make(<stretchy-vector> /* of <init-arg> */);
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
   slot adjs = make(<stretchy-vector> /* of #"sealed", #"abstract", etc. */);
   slot parameter-list :: <parameter-list>;
   slot inferred-param-list :: false-or(<parameter-list>);
end class;

define class <explicit-generic-defn> (<func/gen-definition>, <source-location-mixin>)
   slot vendor-options = make(<stretchy-vector> /* of <vendor-option> */);
   slot sealed-domains = make(<stretchy-vector> /* of <sealed-domain> */);
end class;

define class <implicit-generic-defn> (<func/gen-definition>)
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

define class <explicit-constant-defn> (<const/var-defn>, <source-location-mixin>)
end class;

define class <explicit-variable-defn> (<const/var-defn>, <source-location-mixin>)
end class;


//
// Macros
//

define abstract class <explicit-macro-defn> (<imp/exp-definition>, <source-location-mixin>)
   slot main-rules = make(<stretchy-vector> /* of <rule> */);
   slot aux-rules = make(<stretchy-vector> /* of <aux-rules> */);
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
   slot rules = make(<stretchy-vector> /* of <rule> */);
end class;

define class <rule> (<object>)
   slot pattern :: <pattern>;
   slot template :: <template>;
end class;


//
// Patterns
//

define class <pattern> (<object>)
   slot pattern-lists = make(<stretchy-vector> /* of <pattern-list> */);
end class;

define class <pattern-list> (<object>)
   slot pattern-sequences = make(<stretchy-vector> /* of <pattern-sequence> */);
   slot property-list-pattern :: false-or(<property-list-pattern>);
end class;

define class <pattern-sequence> (<object>)
   slot simple-patterns = make(<stretchy-vector> /* of <simple-pattern> */);
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
   slot patvar-keys = make(<stretchy-vector> /* of <pattern-keyword> */);
   slot patvar-all-keys? :: <boolean>;
end class;

define class <rest-key-pattern> (<property-list-pattern>)
   slot patvar :: <pattern-variable>;
   slot patvar-keys = make(<stretchy-vector> /* of <pattern-keyword> */);
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

define class <fragment> (<source-location-mixin>)
   slot content :: <string>;
end class;

define class <computed-constant> (<fragment>)
end class;

define class <code-fragment> (<fragment>)
end class;

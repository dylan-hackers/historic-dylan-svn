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

define class <constant-defn> (<definition>)
   slot explicit-defn :: <explicit-constant-defn>,
      required-init-keyword: #"explicit";
end class;

define class <variable-defn> (<definition>)
   slot explicit-defn :: <explicit-variable-defn>,
      required-init-keyword: #"explicit";
end class;

define class <macro-defn> (<definition>)
   slot explicit-defn :: <explicit-macro-defn>,
      required-init-keyword: #"explicit";
end class;

define method all-defns (gen :: <generic-defn>) => (defns :: <sequence>)
   let defns = if (gen.explicit-defn) vector(gen.explicit-defn) else #[] end;
   concatenate(defns, gen.implicit-defns)
end method;

define method all-defns (defn :: <definition>) => (defns :: <sequence>)
   vector(defn.explicit-defn)
end method;


//
// Constants and variables
//


define abstract class <const/var-defn> (<documentable-api-element>)
   slot type :: false-or(<type-fragment>) = #f, init-keyword: #"type";
   slot value :: <computed-constant>, required-init-keyword: #"value";
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

define class <computed-constant> (<fragment>)
end class;

define class <type-fragment> (<computed-constant>)
end class;

define class <code-fragment> (<fragment>)
end class;

define class <name-fragment> (<fragment>)
end class;

define class <source-name> (<source-location-mixin>)
   slot source-name :: <string>, required-init-keyword: #"name";
end class;

define method fragment-names (frag :: <fragment>)
=> (names :: <sequence> /* of <source-name> */)
   choose(rcurry(instance?, <source-name>), frag.source-text)
end method;

define method \= (frag1 :: <fragment>, frag2 :: <fragment>)
=> (equal? :: <boolean>)
   (frag1.object-class = frag2.object-class) & (frag1.source-text = frag2.source-text)
end method;

define method \= (name1 :: <source-name>, name2 :: <source-name>)
=> (equal? :: <boolean>)
   case-insensitive-equal?(name1.source-name, name2.source-name)
end method;

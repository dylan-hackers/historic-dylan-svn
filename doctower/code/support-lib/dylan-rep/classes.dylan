module: dylan-rep
synopsis: Classes and methods for representing Dylan classes & slots.


//
// Definition
//


define class <class-defn> (<definition>)
   slot explicit-defn :: <explicit-class-defn>,
      required-init-keyword: #"explicit";
end class;


//
// Classes and slots
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
define class <explicit-class-defn> (<source-location-mixin>)
   slot adjs = make(<stretchy-vector>);
   slot direct-supers = make(<stretchy-vector>);
   slot slots = make(<stretchy-vector>);
   slot init-args = make(<stretchy-vector>);
end class;

define abstract class <slot> (<source-location-mixin>)
end class;

define abstract class <initable-slot> (<slot>)
   slot init-spec :: false-or(<init-spec>) = #f;
end class;

/**
Synopsis: An accessor slot is a slot with accessor methods.

An inherited slot does have accessor methods, but those methods are specialized
on the superclass that defines the slot. All an inherited slot declaration does
is provide a new initial value for the slot and that does not impact the
accessor methods. Thus, the <inherited-slot> class is not a subclass of
<accessor-slot>.

A virtual slot's accessor methods are defined by the programmer. Dylan only
guarantees that there are generic functions to which they can be added. These
generic functions are implicitly defined, like the generic functions created by
"define method" or instance slot accessors, but the programmer is not prohibited
from explicitly defining a generic function for the virtual slot. Thus, the
representation cannot automatically create an <explicit-generic-defn> or an
<implicit-generic-defn> for a virtual slot, but can provide an empty
<generic-defn>.
**/
define abstract class <accessor-slot> (<slot>)
   slot getter :: <generic-defn>;
   slot setter :: false-or(<generic-defn>) = #f;
   slot type :: false-or(<type-fragment>) = #f;
   slot sealed? :: <boolean> = #f, init-keyword: #"sealed";
end class;   

define class <inherited-slot> (<initable-slot>)
end class;

define class <instance-slot> (<accessor-slot>, <initable-slot>)
end class;

define class <class-slot> (<accessor-slot>, <initable-slot>)
end class;

define class <subclass-slot> (<accessor-slot>, <initable-slot>)
end class;

define class <virtual-slot> (<accessor-slot>)
end class;


//
// Initializers
//


define class <init-arg> (<documentable-api-element>)
   slot symbol :: <string>, required-init-keyword: #"symbol";
   slot type :: false-or(<type-fragment>) = #f;
   slot init-spec :: false-or(<init-spec>) = #f;
end class;

// TODO: Can probably get rid of this class and use <code-fragment> directly.
define class <init-spec> (<object>)
   slot code-fragment :: <code-fragment>, required-init-keyword: #"fragment";
end class;



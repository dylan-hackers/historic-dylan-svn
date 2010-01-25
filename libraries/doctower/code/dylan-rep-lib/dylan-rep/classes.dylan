module: dylan-rep
synopsis: Representation of Dylan classes & slots.


define class <class-binding> (<binding>)
   slot explicit-defn :: false-or(<explicit-class-defn>),
      required-init-keyword: #"explicit";
end class;


define class <explicit-class-defn> (<implicit/explicit-defn>)
   /// Adjectives to class. Sequence of <symbol>.
   slot adjectives = make(<stretchy-vector>);
   
   /// Sequence of <type-fragment> comprising superclass list.
   slot direct-supers = make(<stretchy-vector>);
   
   /// Slots in this class. Excludes superclasses. Sequence of <slot>.
   slot slots = make(<stretchy-vector>);
   
   /// Initialization keywords defined by "slot" and "keyword" clauses. Excludes
   /// superclasses. Sequence of <init-arg>.
   slot init-args = make(<stretchy-vector>);
end class;


//
// Slots
//


/// Its source location is a "slot" clause.
define abstract class <slot> (<api-object>)
end class;


define abstract class <initable-slot> (<slot>)
   slot init-spec :: false-or(<code-fragment>) = #f;
end class;


/// Synopsis: An accessor slot is a slot with accessor methods.
define abstract class <accessor-slot> (<slot>)
   slot getter :: <generic-binding>;
   slot setter :: false-or(<generic-binding>) = #f;
   slot type :: false-or(<type-fragment>) = #f;
end class;   


/// An inherited slot has accessor methods, but those methods are specialized on
/// the superclass that defines the slot. All an inherited slot declaration does
/// is provide a new initial value for the slot and that does not impact the
/// accessor methods. Thus, the <inherited-slot> class is not a subclass of
/// <accessor-slot>.
define class <inherited-slot> (<initable-slot>)
end class;


define class <instance-slot> (<accessor-slot>, <initable-slot>)
end class;


define class <class-slot> (<accessor-slot>, <initable-slot>)
end class;


define class <subclass-slot> (<accessor-slot>, <initable-slot>)
end class;


/// A virtual slot's accessor methods are defined by the programmer. Dylan only
/// guarantees that there are generic functions to which they can be added.
/// These generic functions are implicitly defined, like the generic functions
/// created by "define method" or instance slot accessors, but the programmer is
/// not prohibited from explicitly defining a generic function for the virtual
/// slot. Thus, the representation cannot automatically create an
/// <explicit-generic-defn> or an <implicit-generic-defn> for a virtual slot,
/// but can provide an empty <generic-binding>.
define class <virtual-slot> (<accessor-slot>)
end class;


//
// Init arguments
//


/// Its source location is a "keyword" or "slot" clause.
define class <init-arg> (<documentable-api-object>)
   slot symbol :: <string>, required-init-keyword: #"symbol";
   slot type :: false-or(<type-fragment>) = #f;
   slot init-spec :: false-or(<code-fragment>) = #f;
end class;


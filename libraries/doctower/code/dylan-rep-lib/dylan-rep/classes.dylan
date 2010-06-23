module: dylan-rep
synopsis: Representation of Dylan classes & slots.


define class <class-binding> (<binding>)
   slot explicit-defn :: false-or(<explicit-class-defn>) = #f,
      init-keyword: #"explicit";

   /// Sequence of <class-binding> comprising direct superclass list.
   slot effective-supers :: <sequence> = #[];

   /// Sequence of <class-binding> comprising direct subclass list.
   slot effective-subs :: <sequence> = make(<stretchy-vector>);
   
   /// Sequence of <class-binding> that is the class precedence list.
   slot effective-cpl :: <list> = #();

   /// Slot getters in this class, including from superclasses. Sequence of
   /// <generic-binding>.
   slot effective-slots :: <sequence> = make(<stretchy-vector>);
   
   /// Initialization keywords defined by "slot" and "keyword" clauses and "make"
   /// and "initialize" methods, including from superclasses. Sequence of <init-arg>.
   slot effective-init-args :: <sequence> = make(<stretchy-vector>);
   
   /// Sequence of <generic-binding> or <function-binding> that has this class
   /// as a value type.
   slot functions-returning-class :: <sequence> = make(<stretchy-vector>);
   
   /// Sequence of <generic-binding> or <function-binding> that has this class
   /// as an argument type.
   slot functions-on-class :: <sequence> = make(<stretchy-vector>);
end class;


define class <explicit-class-defn> (<implicit/explicit-defn>)
   /// Adjectives to class. Sequence of <symbol>.
   slot adjectives :: <sequence> = make(<stretchy-vector>);
   
   /// Sequence of <type-fragment> comprising superclass list.
   slot direct-supers :: <sequence> = make(<stretchy-vector>);
   
   /// Slots in this class. Sequence of <slot>. Excludes superclasses.
   slot slots :: <sequence> = make(<stretchy-vector>);
   
   /// Initialization keywords defined by "slot" and "keyword" clauses. Sequence
   /// of <init-arg>. Excludes superclasses.
   slot init-args :: <sequence> = make(<stretchy-vector>);
end class;


//
// Slots
//


/// Its source location is a "slot" clause.
define abstract class <slot> (<api-object>)
   slot getter :: <generic-binding>;
end class;


define abstract class <initable-slot> (<slot>)
   slot init-spec :: false-or(<code-fragment>) = #f;
end class;


/// Synopsis: An accessor slot is a slot with accessor methods.
define abstract class <accessor-slot> (<slot>)
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


/// Its source location is a "keyword" or "slot" clause, or a keyword argument
/// of "make" or "initialize".
///
/// Init-arg declarations can come from several sources.
///
/// 1. "Make" methods
/// 2. Class declarations
/// 3. Superclass declarations
/// 4. "Initialize" methods
///
/// In the event of duplicate declarations of an init-arg, we will use the first
/// specified type and init-spec found in the above order. We will not attempt
/// to use the most specific type found among all declarations because our
/// knowledge of the type hierarchy is too fragmentary. We will not consult
/// "make" methods on superclasses because we have no way of knowing if they are
/// called.
///
define class <init-arg> (<documentable-api-object>)
   slot symbol :: <string>, required-init-keyword: #"symbol";
   slot type :: false-or(<type-fragment>) = #f;
   slot init-spec :: false-or(<code-fragment>) = #f;
end class;


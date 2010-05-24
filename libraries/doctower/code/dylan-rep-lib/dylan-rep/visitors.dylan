module: dylan-rep


/// Generic Function: visit-type-fragments
/// Synopsis: Visits <api-object> subclasses and their nested slots that can
/// contain <type-fragment> objects.
///
/// Arguments:
///   object      - The definitions or <api-object> to visit.
///   operation   - A <function> on 'object'. The function is passed the object
///                 and the setter: argument, which is a function that can
///                 replace the object.

define collection-recursive slot-visitor visit-type-fragments
   // Constants and variables
   <constant-binding>,     explicit-defn;
   <variable-binding>,     explicit-defn;
   <const/var-defn>,       type;
   
   // Functions
   <generic-binding>,      explicit-defn, implicit-defns, sealed-domains;
   <function-binding>,     explicit-defn;
   <sealed-domain>,        sealed-types;
   <func/gen-defn>,        param-list, value-list;

   // Parameters and values
   <param-list>,           req-params;
   <key-param-list>,       req-params, key-params;
   <value-list>,           req-values, rest-value;
   <req-param>,            type;
   <key-param>,            type;
   <value>,                type;
   
   // Classes
   <class-binding>,        explicit-defn;
   <explicit-class-defn>,  init-args, slots, direct-supers;

   // Slots and init args
   <slot>,                 getter;
   <accessor-slot>,        getter, setter, type;
   <init-arg>,             type;
   
   // Our goal
   <type-fragment>,        ;
end slot-visitor;

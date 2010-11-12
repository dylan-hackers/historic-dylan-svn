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
   <key-param-list>,       key-params;
   <value-list>,           req-values, rest-value;
   <req-param>,            type;
   <key-param>,            type;
   <value>,                type;
   
   // Classes
   <class-binding>,        explicit-defn;
   <explicit-class-defn>,  init-args, slots, direct-supers;

   // Slots and init args
   <slot>,                 getter;
   <accessor-slot>,        setter, type;
   <init-arg>,             type;
   
   // Our goal
   <type-fragment>,        ;
   
   // Cut recursion
   <string>,               ;
end slot-visitor;


/// Generic Function: visit-api-markup-tokens
/// Synopsis: Visits <documentable-api-object> instances tightly associated with
/// APIs.
///
/// Arguments:
///   object      - The <api-object> to visit.
///   operation   - A <function> on 'object'. The function is passed the object
///                 and the setter: argument, which is a function that can
///                 replace the object.

define collection-recursive slot-visitor visit-api-markup-tokens
   // Our goal
   <documentable-api-object>, ;
   
   // Bindings
   <binding>,              all-defns;
   <class-binding>,        effective-init-args;
   
   // Parameters and values
   <func/gen-defn>,        param-list, value-list;
   <param-list>,           req-params;
   <key-param-list>,       key-params, rest-param;
   <var-param-list>,       rest-param;
   <value-list>,           req-values, rest-value;
end slot-visitor;

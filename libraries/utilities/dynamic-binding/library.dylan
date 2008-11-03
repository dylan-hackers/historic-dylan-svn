module: dylan-user
synopsis: This library supports dynamically-bound variables with dynamic scope
          accessible via the dynamic-binding and dynamic-binding-setter macros.
author: Dustin Voss

define library dynamic-binding
   use dylan;
   export dynamic-binding;
end library;

define module dynamic-binding
   use dylan;
   export with-dynamic-bindings, dynamic-binding, dynamic-binding-setter,
          <binding-not-in-dynamic-scope>, binding-name;
end module;

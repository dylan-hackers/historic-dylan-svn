module: dylan-user
synopsis: This library supports dynamically-bound variables with dynamic scope
          accessible via the dynamic-binding and dynamic-binding-setter macros.
author: Dustin Voss

define library dynamic-binding
   use common-dylan;
   export dynamic-binding;
end library;

define module dynamic-binding
   use dylan;
   use common-dylan, import: { format-to-string };
   export with-dynamic-bindings, dynamic-binding, dynamic-binding-setter,
          <dynamic-binding-access>, <binding-not-in-dynamic-scope>, binding-name;
end module;

module: dylan-user
synopsis: This library defines a macro to recursively visit objects and their
          slots.
author: Dustin Voss

define library slot-visitor
   use dylan;
   use collections, import: { plists };
   export slot-visitor;
end library;

define module slot-visitor
   use dylan;
   use plists, import: { remove-property! };
   export slot-visitor-definer;
end module;

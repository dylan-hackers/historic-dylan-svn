module: dylan-user

define library multimap
  use common-dylan;
  use table-extensions;
  export multimap;
end library;

define module multimap
  use common-dylan;
  use table-extensions;
  export <multimap>, mm-element, mm-element-setter;
end module;

module: dylan-user

define library red-black
  use common-dylan;
  use table-extensions;
  use io;

  export red-black;
end library;

define module red-black
  use common-dylan;
  use table-extensions;
  use format-out;

  export
    <rb-tree>,
    <lt-tree>,
    order,
    insert,
    delete,
    min-key,
    max-key,
    predecessor,
    successor,
    a-key,
    display;
end module;

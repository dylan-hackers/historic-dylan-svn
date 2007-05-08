module: dylan-user

define library b
  use common-dylan;
  use io;
  use a;

  export b;
end library;

define module b
  use common-dylan;
  use format-out;
  use a;

  export <c>;
end module;

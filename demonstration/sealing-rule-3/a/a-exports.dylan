module: dylan-user

define library a
  use common-dylan;
  use io;

  export a;
end library;

define module a
  use common-dylan;
  use format-out;

  export <s>, <t>, m;
end module;

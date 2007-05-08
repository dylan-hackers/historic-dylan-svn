module: dylan-user

define library c
  use common-dylan;
  use io;
  use a;
  use b;
end library;

define module c
  use common-dylan;
  use format-out;
  use a;
  use b;
end module;

module: dylan-user

define library skip-list-test
  use common-dylan;
  use io;
  use skip-list;
  use testworks;
end library;

define module skip-list-test
  use dylan;
  use skip-list;
  use format;
  use format-out;
  use testworks;
end module;

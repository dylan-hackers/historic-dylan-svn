module: dylan-user

define library multi-thread-test
  use dylan;
  use system;
  use memory-manager;

  use io;
end;

define module multi-thread-test
  use dylan;
  use memory-manager;
  use threads;
  use format-out;
end;

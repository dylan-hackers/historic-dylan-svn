module: dylan-user

define library tcp-command-server
  use common-dylan;
  use io;
  use system;
  use network;
end library;

define module tcp-command-server
  use common-dylan;
  use format-out;
  use sockets;
  use date;
  use operating-system;
  use threads;
  use streams;
  use file-system;
  use locators;
end module;

module: dylan-user

define library openssl-wrapper
  use common-dylan;
  use dylan;
  use c-ffi;
  use io;
  use network;
end library;

define module openssl-wrapper
  use common-dylan;
  use simple-io;
  use dylan;
  use c-ffi;
//  use unix-sockets, import: { <C-buffer-offset> };
//  use sockets, import: { buffer-offset, interruptibe-system-call };
end module;

module: dylan-user

define library openssl-wrapper
  use common-dylan;
  use dylan;
  use c-ffi;
  use io;
  use network;
//  use streams;
end library;

define module openssl-wrapper
  use common-dylan;
  use simple-io;
  use format;
  use dylan;
  use c-ffi;
  use streams;
  use unix-sockets, import: { <C-buffer-offset> };
//  use sockets, import: { buffer-offset, interruptibe-system-call };
  use sockets;
  use streams-internals, import:
    { stream-input-buffer, stream-input-buffer-setter,
      stream-output-buffer, stream-output-buffer-setter,
      stream-direction, stream-direction-setter,
      accessor-preferred-buffer-size,
      platform-accessor-class, 
      accessor-open, accessor-close,
      accessor, accessor-setter, new-accessor,
      accessor-read-into!, accessor-write-from };
//  use unix-sockets;
  //use sockets-internals;
end module;

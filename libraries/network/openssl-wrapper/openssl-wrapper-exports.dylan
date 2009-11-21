module: dylan-user

define library openssl-wrapper
  use common-dylan;
  use dylan;
  use c-ffi;
  use io;
end library;

define module openssl-wrapper
  use common-dylan;
  use dylan;
  use c-ffi;
  use format-out;
  use standard-io;
  use streams-protocol, import: { force-output };
end module;

module: dylan-user

define library zlib
  use common-dylan;
  use c-ffi;
  export zlib;
end library;

define module zlib
  use common-dylan;
  use c-ffi;
  export compress;
end module;

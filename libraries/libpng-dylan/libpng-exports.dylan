module: dylan-user


define module libpng
  use common-dylan;
  use extensions, import: { <double-integer> };
  use melange-support;
  use streams;
  use format-out;
  use libpng-internal;

  export read-png;
end module libpng;

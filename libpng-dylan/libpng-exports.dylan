module: dylan-user
author: Andreas Bogk <andreas@andreas.org>
copyright: (C) Andreas Bogk, under modified BSD license

define module libpng
  use common-dylan;
  use extensions, import: { <double-integer> };
  use melange-support;
  use streams;
  use format-out;
  use libpng-internal;

  export read-png;
end module libpng;

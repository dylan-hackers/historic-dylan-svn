module: dylan-user
author: Andreas Bogk <andreas@andreas.org>
copyright: (C) Andreas Bogk, under modified BSD license

define module libpng
  use common-dylan;
  use extensions, import: { <double-integer> };
  use melange-support;
  use streams;
  use streams-internals, import: { accessor };
  use file-system;
  use format-out;
  use libpng-internal;
  use io-internals, import: { file-descriptor };

  export read-png;
end module libpng;

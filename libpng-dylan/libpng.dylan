module: dylan-user

define library libpng
  use common-dylan;
  use io;
  use streams;
  use melange-support;
end library libpng;

define module libpng
  use common-dylan;
  use extensions, import: { <double-integer> };
  use melange-support;
  use streams;
  use format-out;
end module libpng;

  
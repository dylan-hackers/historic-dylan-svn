module: dylan-user

define library libpng
  use common-dylan;
  use melange-support;
end library libpng;

define module libpng
  use common-dylan;
  use extensions, import: { <double-integer> };
  use melange-support;
end module libpng;

  
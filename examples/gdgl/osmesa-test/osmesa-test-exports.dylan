module: dylan-user

define library osmesa-test
  use common-dylan;
  use io;

  use opengl;
end library;

define module osmesa-test
  use common-dylan;
  use format-out;

  use opengl;
  use opengl-osmesa;
end module;

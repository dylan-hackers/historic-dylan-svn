library: gtk-hello-world
module: dylan-user

define library gtk-hello-world
  use dylan;
  use melange-support;
  use streams;
  use format;
  use standard-io;
  use gtk-base;
end library;

define module gtk-hello-world
  use dylan;
  use extensions;
  use melange-support;
  use system;
  use streams;
  use format;
  use standard-io;
  
  use gtk-base;
end module;

module: dylan-user

define library sieve-fixed
  use dylan;
end library;

define module sieve-fixed
  use dylan;
  use extensions, exclude: {main};
  use cheap-io;
end module;

module: dylan-user

define library robber
  use common-dylan;
  use io;
end library;

define module world
  use common-dylan;
  use format-out;
  export <world>;
end module;

define module robber
  use common-dylan;
  use format-out;
  use world;
end module;

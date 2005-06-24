module: dylan-user

define library cop
  use common-dylan;
  use io;
end library;

define module world
  use common-dylan;
  use format-out;
  export <world>;
end module;

define module cop
  use common-dylan;
  use format-out;
  use world;
end module;

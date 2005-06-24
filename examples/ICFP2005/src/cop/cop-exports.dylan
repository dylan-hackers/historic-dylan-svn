module: dylan-user

define library cop
  use common-dylan;
  use io;
  use regular-expressions;
  use collection-extensions;
  use system;
end library;

define module world
  use common-dylan;
  use format-out;
  export <world>;
end module;

define module cop
  use common-dylan;
  use format-out;
  use standard-io;
  use streams;
  use subseq;
  use regular-expressions, exclude: { split };
  use file-system;

  //use world;
end module;

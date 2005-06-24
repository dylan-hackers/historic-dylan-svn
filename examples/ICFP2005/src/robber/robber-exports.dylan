module: dylan-user

define library robber
  use common-dylan;
  use io;

  use world;
end library;

define module robber
  use common-dylan;
  use simple-random;
  use format-out;
  use standard-io;
  use streams;
  use format, import: { format };
  use world;
end module;

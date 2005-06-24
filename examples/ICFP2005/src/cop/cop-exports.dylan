module: dylan-user

define library cop
  use common-dylan;
  use io;
  use system;
  use world;
end library;


define module cop
  use common-dylan;
  use format-out;
  use standard-io;
  use streams;
  use simple-random;
  use format, import: { format };

  use world;
end module;

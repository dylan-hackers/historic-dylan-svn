module: dylan-user

define library world
  use common-dylan;
  use io;
  use system;
  use regular-expressions;
  use collection-extensions;
  use streams;
  export world;
end library;


define module world
  use common-dylan;
  use format-out;
  use standard-io;
  use regular-expressions, import: {regexp-position};
  use subseq, import: {subsequence};
  use streams, import: {read-line};
//  use simple-random;
//  use format, import: { format };

  export <world>, <inform>,
    world-skeleton, players, location, edges, type, name,
    read-world-skeleton, read-world, <parse-error>,
    find-player, find-possible-locations;
end module;

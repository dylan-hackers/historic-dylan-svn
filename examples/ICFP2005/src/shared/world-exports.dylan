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
  use streams;
//  use simple-random;
  use format, import: { format };

  export <world>, <inform>, <plan>,
    world-skeleton, world-players, world-edges, world-number,
    type, name, location,
    read-world-skeleton, read-world, <parse-error>,
    find-player, find-possible-locations,
    re, ws-re, name-re, node-tag, edge-type-re, number-re, negnumber-re, ptype-re,
    collect, dbg, send,
    world, bot, certainty;

end module;

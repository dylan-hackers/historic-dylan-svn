module: dylan-user
synopsis: Dylan Hackers entry in the Fifth Annual (2002) ICFP Programming Contest
copyright: this program may be freely used by anyone, for any purpose

define library icfp2002
  use common-dylan;
  use io;
  use collection-extensions;
  use string-extensions;
//  use time; // gabor does not have this present at the moment
  use garbage-collection;
  use network;
end library;


define module board
  use common-dylan;
  use streams;
  use format-out;
  use print, import: {print-object};
  
  export
    <state>, board, robots, packages, packages-at,
    <board>,
    <coordinate>, <point>, x, y, point,
    send-board,
    width, height, passable?,
    <terrain>, <wall>, <water>, <base>, <space>,
    <package>, id, weight, location, dest, at-destination?, free-packages,
    <robot>, id, capacity, inventory, location, capacity-left,
    add-robot,
    find-robot,
    add-package;
end module board;


define module path
  use common-dylan;
  use board;

  // For debugging only. Sorry.
  use format-out;
  use standard-io;
  use streams, export: all;

  export <point-list>, find-path;
end module path;

define module command
  use common-dylan;

  export
    $north,
    $south,
    $east,
    $west,
    <direction>,
    <command>, bid,
    <move>, direction,
    <pick>, package-ids,
    <drop>;
end module command;

define module messages
  use common-dylan;
  use standard-io;
  use streams;
  use character-type, import: {digit?};
  use board;
  use command;

  export
    //    <message-error>,
    //    message-error,
    //    add-error,
    // send routines
    send-player,  // This sends the "Player" message to the server to
    //initialize.
    send-command,
    // receive routines
    receive-initial-setup; // Reads initial board plus self robot, with robot
  // positions. Does it all.
  // receive-initial-setup calls:
  //    receive-integer,
  //    receive-string,
  //    receive-board-layout,  // Reads initial board layout, w/o robot positions.
  //    receive-client-configuration, // Reads our initial status.
  //    receive-robot-location,
  //    receive-initial-robot-positions; // Updates board with robot positions.
end module messages;

define module client
  use common-dylan;
  use board;
  use command;
  use path;

  export
    <robot-agent>,
    <dumbot>, <dumber-bot>,
    <dumber-bot>,
    generate-next-move;
end module client;


define module server
  use board;
  
end module server;

define module icfp2002
  use common-dylan, exclude: {string-to-integer}, export: all;
  use format-out;
  use format;
  use subseq;
  use streams, export: all;
  use standard-io;
  use string-conversions, import: {string-to-integer};
  use extensions, import: {report-condition};
  // use time;
  use garbage-collection;
  use network;
  use messages;
  use board;
  use client;

  // For testing only. Sorry.
  use path;
end module;

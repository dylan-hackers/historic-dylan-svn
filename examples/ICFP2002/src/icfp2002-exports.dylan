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

define module icfp2002
  use common-dylan, exclude: {string-to-integer}, export: all;
  use format-out;
  use format;
  use subseq;
  use streams, export: all;
  use standard-io;
  use string-conversions, import: {string-to-integer};
  use extensions, import: {report-condition};
//  use time;
  use garbage-collection;
  use network;
end module;


define module board
  use icfp2002;
  
  export
    <state>, robots, packages,
    <board>, <coordinate>,
    send-board, receive-board,
    width, height,
    <terrain>, <wall>, <water>, <base>, <space>,
    <package>, weight, x, y, dest-x, dest-y,
    <robot>, capacity, x, y, capacity-left,
    add-robot,
    add-package;
end module board;

define module command
  use icfp2002;

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
  use icfp2002, exclude: {string-to-integer};
  use string-conversions, import: {string-to-integer};
  use standard-io;
  use character-type, import: {digit?};
  use board;
  use command;

  export
    <message-error>,
    message-error,
    add-error,
    // send routines
    send-player,  // This sends the "Player" message to the server to
                  //initialize.
    send-command,
    // receive routines
    receive-initial-setup, // Reads initial board plus self robot, w/ robot
                           // positions. Does it all.
    // receive-initial-setup calls:
    receive-integer,
    receive-string,
    receive-board-layout,  // Reads initial board layout, w/o robot positions.
    receive-client-configuration, // Reads our initial status.
    receive-robot-location,
    receive-initial-robot-positions; // Updates board with robot positions.
end module messages;

define module client
  use common-dylan;
  use board;
  use command;

  export <robot-agent>
end module client;


define module server
  use board;
  
end module server;

module: dylan-user
synopsis: Dylan Hackers entry in the Fifth Annual (2002) ICFP Programming Contest
copyright: this program may be freely used by anyone, for any purpose

define library icfp2002
  use common-dylan;
  use io;
  use sequence-utilities;
//  use collection-extensions;
//  use string-extensions;
//  use time; // gabor does not have this present at the moment
//  use garbage-collection;
  use network;
end library;

define module utils
  use common-dylan;
  use streams;
  use format;
  use format-out;
  use standard-io;

  export
    *debug*, debug, force-format;
end module utils;

define module fd-compat
  use common-dylan;
  use sequence-utilities;
  use sockets;
  use streams;
  

  export split, tcp-client-connection, digit?, fd-read;
end module fd-compat;

define module board
  use common-dylan;
  use streams;
  use format;
  use print, import: {print-object};
  
  export
    <state>, board, robots, packages, packages-at, bases, bases-setter,
    <board>,
    <coordinate>, <point>, x, y, point,
    send-board, receive-board,
    width, height, passable?,
    <terrain>, <wall>, <water>, <base>, <space>,
    <package>, id, weight, location, dest, at-destination?, free-packages,
    <robot>, id, capacity, inventory, location, capacity-left,
    add-robot,
    add-package;
end module board;


define module path
  use common-dylan;
  use io;
  use standard-io;
  use streams;
  use format;
  use board;
  use utils;

  export <point-list>, distance-cost, find-path;
end module path;

define module command
  use common-dylan;
  use board, import: { <point> };

  export
    $north,
    $south,
    $east,
    $west,
    <direction>,
    <command>, bid,
    <move>, direction,
    <pick>, package-ids,
    <drop>,
    <transport>;
end module command;

define module messages
  use common-dylan;
  use standard-io;
  use streams;
  use format;
  use fd-compat;
//  use character-type, import: {digit?};
  use board;
  use command;
  use utils;

  export
//    <message-error>,
//    message-error,
//    add-error,
    // send routines
    send-player,  // This sends the "Player" message to the server to
                  //initialize.
    send-command,
    // receive routines
    receive-server-packages,
    receive-server-command-reply,
    receive-initial-setup; // Reads initial board plus self robot, w/ robot
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
  use utils;

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
  use common-dylan, export: all;
  use format-out;
  use format;
  use fd-compat;
//  use subseq;
  use streams, export: all;
  use standard-io;
//  use string-conversions, import: {string-to-integer};
//  use extensions, import: {report-condition};
//  use time;
//  use garbage-collection;
//  use network;
  use messages;
  use board;
  use command;
  use client;
  use path;
  use utils;

end module;

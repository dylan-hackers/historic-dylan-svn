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
  use icfp2002, export: all;
  
  export <board>, send-board;
end module board;

define module messages
  use icfp2002;
  use standard-io;
  use string-conversions;
  use board;
  use robot;

  export
    <message-error>,
    message-error,
    add-error,
    // send routines
    send-player,  // This sends the "Player" message to the server.
    // receive routines
    receive-initial-setup, // Reads initial board plus self robot, w/ robot
                           // positions. Does it all.
    // receive-initial-setup calls: 
    receive-board-layout,  // Reads initial board layout, w/o robot positions.
    receive-client-configuration, // Reads our initial status.
    receive-initial-robot-positions; // Updates board with robot positions.
end module messages;

define module client
  use board;
  
end module client;

define module server
  use board;
  
end module server;





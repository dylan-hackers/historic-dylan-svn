Module:   dylan-user
Synopsis: Koala example code
Author:   Carl Gay

define library koala-basics
  use dylan;
  use common-extensions;
  use io;
  use network;
  use locators;
  use koala;
end;


define module koala-basics
  use dylan;
  use threads;      // from dylan lib
  use common-extensions, exclude: { format-to-string };
  use locators;     // from locators lib
  use format;       // from IO lib
  use streams;      // from IO lib
  use sockets, import: { <tcp-socket> };  // from network lib
  use dsp;          // from koala lib
end;


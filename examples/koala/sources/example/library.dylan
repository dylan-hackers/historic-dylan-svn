Module:   dylan-user
Synopsis: Koala example code
Author:   Carl Gay

define library koala-example
  use functional-dylan;
  use io;
  use network;
  use system;
  use koala;
  export koala-example;
end library koala-example;


define module koala-example
  use functional-dylan;
  use format;
  use format-out;
  use streams;
  use threads;
  use sockets, import: { <tcp-socket> };
  use dsp;
  use locators;
end;


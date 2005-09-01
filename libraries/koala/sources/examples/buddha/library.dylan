module: dylan-user
author: Hannes Mehnert <hannes@mehnert.org>

define library buddha
  use common-dylan;
  use io;
  use koala, import: { koala };
  use dood;
  use string-extensions, import: { character-type };
  use regular-expressions;
  use network;
  export buddha;
end;

define module buddha
  use common-dylan;
  use threads;
  use format-out;
  use format, import: { format };
  use print, import: { print-object };
  use koala, exclude: { print-object };
  use streams;
  use standard-io;
  use character-type, import: { hex-digit? };
  use dood;
  use regular-expressions;
  use sockets, import: { <tcp-socket>, <internet-address> };
end;

module: dylan-user
author: Hannes Mehnert

define library buddha
  use common-dylan;
  use io;
  use koala, import: { dsp };
  use dood;
  use string-extensions, import: { character-type };
  export buddha;
end;

define module buddha
  use common-dylan;
  use format-out;
  use format, import: { format };
  use print, import: { print-object };
  use dsp, exclude: { split, print-object };
  use streams;
  use standard-io;
  use character-type, import: { hex-digit? };
  use dood;
end;

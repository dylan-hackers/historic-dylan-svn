Module:   dylan-user
Synopsis: Koala example code
Author:   Carl Gay

define library koala-basics
  use dylan;
  use common-dylan,
    import: { common-extensions };
  use io,
    import: { format, streams };
  use system,
    import: { locators, threads };
  use koala,
    import: { dsp };
end;


define module koala-basics
  use dylan;
  use threads;
  use common-extensions,
    exclude: { format-to-string, split };
  use locators;
  use format;
  use streams;
  use dsp;
end;


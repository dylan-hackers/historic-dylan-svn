Module: dylan-user

define library web-objects-example
  use common-dylan;
  use io;
  use koala;
end;

define module web-objects-example
  use common-dylan;
  use streams, import: { write };
  use dsp;           // from the koala library
end;



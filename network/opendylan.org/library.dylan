Module: dylan-user
Synopsis: Web back-end for opendylan.org
Author: Carl Gay

define library opendylan-dot-org
  use common-dylan;
  use koala,  import: { dsp, koala };
  use uri;
end;


define module opendylan-dot-org
  use common-dylan;
  use dsp;
  use koala;
  use uri;
end;


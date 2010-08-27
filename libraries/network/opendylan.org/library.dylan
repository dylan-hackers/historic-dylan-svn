Module: dylan-user
Synopsis: Web back-end for opendylan.org
Author: Carl Gay

define library opendylan-dot-org
  use common-dylan;
  use dsp;
  use io;
  use koala;
  use system;
  use uri;
  use wiki;
end;


define module opendylan-dot-org
  use common-dylan;
  use dsp;
  use koala;
  use locators,
    exclude: {
      <http-server>,    // sheesh
      <url>
      };
  use streams;
  use standard-io;
  use uri;
  use wiki;
end;


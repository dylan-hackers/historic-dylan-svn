Module:   dylan-user
Synopsis: Brwose FD environment objects
Author:   Andreas Bogk

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

  use environment-protocols;
  use environment-reports;
  use environment-manager;
  use source-control-manager;

  use dfmc-environment-projects;

  use source-records;
  use release-info;
  use regular-expressions;

end;


define module code-browser
  use dylan;
  use threads;
  use common-extensions,
    exclude: { format-to-string, split };
  use locators;
  use format;
  use streams;
  use dsp;
  use regular-expressions, import: { regexp-replace };
  use source-records;
  use source-records-implementation;
  use environment-protocols,
    exclude: { <singleton-object>, 
               application-filename,
               application-arguments };
  use release-info;
end;


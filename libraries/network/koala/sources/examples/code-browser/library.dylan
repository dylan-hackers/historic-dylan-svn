Module:   dylan-user
Synopsis: Brwose FD environment objects
Author:   Andreas Bogk

define library code-browser
  use dylan;
  use common-dylan,
    import: { common-extensions };
  use io,
    import: { format, format-out, streams };
  use system,
    import: { locators, threads };
  use koala,
    import: { dsp };

  use environment-protocols;
  //use environment-reports;
  use environment-manager;
  use source-control-manager;

  use dfmc-environment-projects;

  use source-records;
  use release-info;
  use regular-expressions;
  use graphviz-renderer;
//use environment-deuce;
end;


define module code-browser
  use dylan;
  use threads;
  use common-extensions,
    exclude: { format-to-string };
  use locators;
  use format;
  use format-out;
  use streams;
  use dsp, exclude: { split };
  use regular-expressions, import: { regexp-replace };
  use source-records;
  use source-records-implementation;
  use environment-protocols,
    exclude: { <singleton-object>, 
               application-filename,
               application-arguments };
  use release-info;
  use graphviz-renderer;
//  use environment-deuce;
end;


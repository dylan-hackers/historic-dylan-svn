module: dylan-user

define library dfmc-visualization-app
  use dylan;
  use io;

  use system;
  use registry-projects;
  use release-info;
  use environment-protocols;
  use projects;
  use dfmc-environment-projects;
  use dfmc-core;
  use dfmc-management;
  use dfmc-typist;
  use dfmc-optimization;

  use dfmc-visualization;
end;

define module dfmc-visualization-app
  use dylan;
  use format-out;

  use threads, import: { dynamic-bind };
  use file-system;
  use locators;
  use projects;
  use registry-projects;
  use release-info;
  use operating-system, exclude: { load-library };
  use environment-protocols,
    import: { find-project, open-project-compiler-database, project-warnings };
  use dfmc-environment-projects;
  use dfmc-core;
  use dfmc-management;
  use dfmc-typist;
  use dfmc-optimization;

  use dfmc-visualization;
end;

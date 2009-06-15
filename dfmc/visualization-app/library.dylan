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
  use dfmc-browser-support;

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
  use projects-implementation, import: { project-build-settings, project-current-compilation-context };
  use release-info;
  use operating-system, exclude: { load-library };
  use environment-protocols,
    import: { find-project, open-project-compiler-database, project-warnings, project-proxy };
  use dfmc-environment-projects;
  use dfmc-core;
  use dfmc-management;
  use dfmc-typist;
  use dfmc-optimization;
  use dfmc-project-compilation, import: { compilation-context-project };

  use dfmc-visualization;
end;

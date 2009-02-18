module: dylan-user

define library dfmc-typist-tests
  use common-dylan;
  use io;
  use testworks;

  use dfmc-core;
  use dfmc-typist;
  use dfmc-management;
  use dfmc-debug-back-end;
  use dfmc-optimization; //to get program-note classes
  use source-records;

  use projects;
  use environment-protocols;
  use dfmc-environment-projects;
  use dfmc-visualization;
end library;

define module dfmc-typist-tests
  use common-dylan;
  use threads, import: { dynamic-bind };
  use format-out;
  use testworks;

  use dfmc-core;
  use dfmc-typist;
  use dfmc-management;
  use dfmc-debug-back-end;
  use dfmc-optimization;

  use source-records;
  use projects;
  use projects-implementation,
    import: { project-current-compilation-context,
              source-record-contents,
              project-current-source-records };
  use environment-protocols,
    import: { find-project, open-project-compiler-database, project-warnings };
  use dfmc-environment-projects; //needed for find-project
  use dfmc-visualization;
end module;

module: dfmc-visualization-app

define constant $tests = make(<stretchy-vector>);

begin
  let if-nested
    = "define method if-nested (x, y, z)\n"
      "  if (x == 1)\n"
      "    if (y == 1)\n"
      "      if (z == 1)\n"
      "        \"all equal\";\n"
      "      else\n"
      "        \"x and y equal\";\n"
      "      end;\n"
      "    elseif (z == 2)\n"
      "      \"y + 1 is z\";\n"
      "    else\n"
      "      \"all different\";\n"
      "    end;\n"
      "  end;\n"
      "end;";
  add!($tests, pair(#"if-nested", if-nested));
  
  let if-instance
    = "define method if-instance ()\n"
      "  let a :: <integer> = 23;\n"
      "  let b :: <integer> = 42;\n"
      "  if (instance?(a, <string>))\n"
      "    a := a * b;\n"
      "  else\n"
      "    a := a + b;\n"
      "  end;\n"
      "  a;\n"
      "end;\n";
  add!($tests, pair(#"if-instance", if-instance));

  let if-simple
    = "define method if-simple\n"
      " (a :: <integer>, b :: <integer>)\n"
      " => (res :: <integer>)\n"
      "  if (a == 23)\n"
      "    1 + a + b;\n"
      "  else\n"
      "    42 + 10;\n"
      "  end;\n"
      "end;";
  add!($tests, pair(#"if-simple", if-simple));

  let if-assign0
    = "define method if-assign0 (a) => (b)\n"
      "  let c = 0;\n"
      "  if (a == 42)\n"
      "    c := 1;\n"
      "  else\n"
      "    c := 2;\n"
      "  end;\n"
      "  c;\n"
      "end;";
  add!($tests, pair(#"if-assign0", if-assign0));

  let if-assign1
    = "define method if-assign1 (a) => (b)\n"
      "  let c = 0;\n"
      "  if (a == 42)\n"
      "    c := 1;\n"
      "  end;\n"
      "  c;\n"
      "end;";
  add!($tests, pair(#"if-assign1", if-assign1));


  let if-assign2
    = "define method if-assign2 (a) => (b)\n"
      "  let c = 0;\n"
      "  if (a == 42)\n"
      "    c := 1;\n"
      "  else\n"
      "    if (a == 43)\n"
      "      c := 2;\n"
      "    end;\n"
      "  end;\n"
      "  c;\n"
      "end;";
  add!($tests, pair(#"if-assign2", if-assign2));

  let common-sub =
    "define method common-subexpression (a, b)\n"
    "  values(a + b, (a + b) * b);\n"
    "end;";
  add!($tests, pair(#"common-subexpression", common-sub));

  let common-sub2 =
    "define method common-subexpression2\n"
    " (a :: <integer>, b :: <integer>)\n"
    "  values(a + b, (a + b) * b);\n"
    "end;";
  add!($tests, pair(#"common-subexpression2", common-sub2));

  let whil-true =
    "define method while-true-loop (x, y, z)\n"
    "  while(#t)\n"
    "    1 + 2;\n"
    "  end;\n"
    "end;";
  add!($tests, pair(#"while-true-loop", whil-true));

  let lfor =
    "define method for-loop (x, y, z)\n"
    "  for (i from 0 below 20)\n"
    "    x := y + 1;\n"
    "  end;\n"
    "end;";
  add!($tests, pair(#"for-loop", lfor));

  let whill =
    "define method while-loop ()\n"
    "  let i = 0;\n"
    "  while(i < 42)\n"
    "    i := i + 1;\n"
    "  end;\n"
    "end;";
  add!($tests, pair(#"while-loop", whill));

  let whill2 =
    "define method while-loop2 ()\n"
    "  let j = 5;\n"
    "  let i = 0;\n"
    "  while(i < 42)\n"
    "    i := i + j;\n"
    "  end;\n"
    "  i\n"
    "end;";
  add!($tests, pair(#"while-loop2", whill2));

  let whilln =
    "define method while-loop-nested ()\n"
    "  let i = 0;\n"
    "  while(i < 42)\n"
    "    i := i + 1;\n"
    "    while (i < 20)\n"
    "      i := i * i;\n"
    "    end;\n"
    "  end;\n"
    "end;";
  add!($tests, pair(#"while-loop-nested", whilln));

  let blte =
    "define method block-test (x)\n"
    "  block(t)\n"
    "    t();\n"
    "  end;\n"
    "end;";
  add!($tests, pair(#"block-test", blte));

  let ble =
    "define method block-exception (x)\n"
    "  block()\n"
    "    x := x * x;\n"
    "  exception (c :: <condition>)\n"
    "    x := 0;\n"
    "  end;\n"
    "end;";
  add!($tests, pair(#"block-exception", ble));

  let blcl =
    "define method block-cleanup (x, y, z)\n"
    "  block(t)\n"
    "    if (x == 42)\n"
    "      t();\n"
    "    end;\n"
    "    x := 20;\n"
    "    y := 42 * x;\n"
    "  cleanup\n"
    "    x := y;\n"
    "  end;\n"
    "end;";
  add!($tests, pair(#"block-cleanup", blcl));

  let db =
    "define method dyn-bind (x, y, z)\n"
    "  let t = 42;\n"
    "  dynamic-bind(t = 0)\n"
    "    x := t * t;\n"
    "  end;\n"
    "  y := t + t;\n"
    "  values(x, y);\n"
    "end;";
  add!($tests, pair(#"dyn-bind", db));

  let mm =
    "define method mymap (A, B)\n"
    " (fun :: A => B, as :: limited(<collection>, of: A))\n"
    " => (bs :: limited(<collection>, of: B))\n"
    "  map(fun, as);\n"
    "end;\n";
  add!($tests, pair(#"mymap", mm));

  let tc =
    "define method tail-call (a :: <integer>)\n"
    "  if (a == 0)\n"
    "    0;\n"
    "  else\n"
    "    foo(a - 1);\n"
    "  end;\n"
    "end;\n";
  add!($tests, pair(#"tail-call", tc));

  let gt0 =
    "define method gradual-typing-test0 ()\n"
    "  local method f (a :: <integer>) => (b :: <integer>)\n"
    "          a;\n"
    "        end;\n"
    "  local method g (a :: <integer>) => (b :: <boolean>)\n"
    "          even?(a);\n"
    "        end;\n"
    "  f(g(1));\n"
    "end;\n";
  add!($tests, pair(#"gradual-typing-test0", gt0));

  let gt1 =
    "define method gradual-typing-test1 ()\n"
    "  local method x (a) a(1) end;\n"
    "  x(1);\n"
    "end;\n";
  add!($tests, pair(#"gradual-typing-test1", gt1));

/*
  let gt2 =
    "define method gradual-typing-test2 ()\n"
    "  local method f\n"
    "         (a :: <object> => <integer>,\n"
    "          b :: <integer> => <object>)\n"
    "         => (res :: <integer>)\n"
    "          2\n"
    "       end,\n"
    "       method y (a :: <object>)\n"
    "         y\n"
    "       end;\n"
    "  f(y, y);\n"
    "end;\n";
  add!($tests, pair(#"gradual-typing-test2", gt2));
*/

  let bin0 =
    "define method binding0 ()\n"
    "  let a = 42;\n"
    "  let b = 455;\n"
    "  a + b;\n"
    "end;\n";
  add!($tests, pair(#"binding0", bin0));

  let bin1 =
    "define method binding1 ()\n"
    "  let a :: <integer> = 42;\n"
    "  let b = 455;\n"
    "  a + b;\n"
    "end;\n";
  add!($tests, pair(#"binding1", bin1));

  let ass1 =
    "define method assignment1 ()\n"
    "  let a = 42;\n"
    "  let b = 455;\n"
    "  a := b + 1;\n"
    "  b := \"foo\";\n"
    "end;\n";
  add!($tests, pair(#"assignment1", ass1));

  let ass0 =
    "define method assignment0 ()\n"
    "  let a = 42;\n"
    "  let b = 455;\n"
    "  a := b + 1;\n"
    "end;\n";
  add!($tests, pair(#"assignment0", ass0));

  let ass2 =
    "define method assignment2 ()\n"
    "  let a = 42;\n"
    "  let b = 455;\n"
    "  a := b + 1;\n"
    "  b := \"foo\";\n"
    "  a := concatenate(b, b);\n"
    "end;\n";
  add!($tests, pair(#"assignment2", ass2));

  let ass3 =
    "define method assignment3 ()\n"
    "  let a :: <integer> = 42;\n"
    "  let b :: type-union(<integer>, <string>) = 455;\n"
    "  a := b + 1;\n"
    "  b := \"foo\";\n"
    "end;\n";
  add!($tests, pair(#"assignment3", ass3));

  let ass4 =
    "define method assignment4 ()\n"
    "  let a = 42;\n"
    "  let b = 455;\n"
    "  a := b + 1;\n"
    "  b := \"foo\";\n"
    "  a := a + 1;\n"
    "  a := a + 2;\n"
    "  a := a + a;\n"
    "  b := a - 4;\n"
    "  values(a + 2, b - 3);\n"
    "end;\n";
  add!($tests, pair(#"assignment4", ass4));

  let ass5 =
    "define method assignment5 ()\n"
    "  let a :: <integer> = 42;\n"
    "  let b = 455;\n"
    "  a := b + 1;\n"
    "  b := \"foo\";\n"
    "  a := a + 1;\n"
    "  values(a + 2, b);\n"
    "end;\n";
  add!($tests, pair(#"assignment5", ass5));

  let ass6 =
    "define method assignment6 ()\n"
    "  let a :: <integer> = 42;\n"
    "  let b = 455;\n"
    "  a := b + 1;\n"
    "  a := \"foo\";\n"
    "  a := a + 1;\n"
    "  values(a + 2, b - 3);\n"
    "end;\n";
  add!($tests, pair(#"assignment6", ass6));

  let mymap2 =
    "define method mymap2 (c :: <list>)\n"
    " => (c :: <list>)\n"
    "  local method ma (x :: <integer>) => (y)\n"
    "          x + 1;\n"
    "        end;\n"
    "  if (c.empty?)\n"
    "    #()\n"
    "  else\n"
    "    add!(mymap2(c.tail), ma(c.head));\n"
    "  end;\n"
    "end;\n";
  add!($tests, pair(#"mymap2", mymap2));

  let poly0 =
    "define method poly0 ()\n"
    "  local method mymap (A, B)\n"
    "   (fun :: A => B, l :: limited(<vector>, of: A))\n"
    "   => (res :: limited(<vector>, of: B))\n"
    "    if (l.empty?)\n"
    "      #[];\n"
    "    else\n"
    "      add(mymap(fun, copy-sequence(l, start: 1)),\n"
    "          fun(l.first));\n"
    "    end;\n"
    "  end;\n"
    "  mymap(method(x) x + 1 end, #[1, 2, 3]);\n"
    "end;";
  add!($tests, pair(#"poly0", poly0));

  let id0 =
    "define method id0 ()\n"
    "  local method id (A) (x :: A) => (y :: A)\n"
    "    x;\n"
    "  end;\n"
    "  id(42)\n"
    "end;";
  add!($tests, pair(#"id0", id0));

  let id1 =
    "define method id1 ()\n"
    "  local method id (A) (x :: A) => (y :: A)\n"
    "    x;\n"
    "  end;\n"
    "  values(id(42), id(\"foo\"))\n"
    "end;";
  add!($tests, pair(#"id1", id1));
end;

define function callback-handler (#rest args)
  format-out("%=\n", args);
end function callback-handler;

begin
  let project = find-project("dylan");
  open-project-compiler-database(project,
                                 warning-callback: callback-handler,
                                 error-handler: callback-handler);
  with-library-context (dylan-library-compilation-context())
    without-dependency-tracking
      let vis = make(<dfmc-graph-visualization>, id: #"Dylan-Graph-Visualization");
      connect-to-server(vis);
      for (test in $tests)
        write-to-visualizer(vis, list(#"source", test.head, test.tail));
      end;
      vis.dfm-report-enabled? := #f;
      block()
        while (#t)
          let res = read-from-visualizer(vis); //expect: #"compile" "source"
          if (res[0] == #"compile")
            block()
              dynamic-bind (*progress-stream*           = #f,  // with-compiler-muzzled
                            *demand-load-library-only?* = #f)
                compile-template(res[1], compiler: curry(visualizing-compiler, vis));
              end;
            exception (e :: <condition>)
              format-out("received exception while compiling: %=\n", e);
            end;
          end;
        end;
      exception (e :: <condition>)
        format-out("received exception: %=\n", e);
      end;
    end;
  end;
end;
            
define function list-all-package-names ()
  let res = #();
  local method collect-project
            (dir :: <pathname>, filename :: <string>, type :: <file-type>)
          if (type == #"file" & filename ~= "Open-Source-License.txt")
            if (last(filename) ~= '~')
              unless (any?(curry(\=, filename), res))
                res := pair(filename, res);
              end;
            end;
          end;
        end;
  let regs = find-registries($machine-name, $os-name);
  let reg-paths = map(registry-location, regs);
  for (reg-path in reg-paths)
    if (file-exists?(reg-path))
      do-directory(collect-project, reg-path);
    end;
  end;
  res;
end;
/*
begin
  let projects = list-all-package-names();
  let vis = make(<dfmc-graph-visualization>, id: #"Dylan-Graph-Visualization");
  connect-to-server(vis);
  for (project in projects)
    write-to-visualizer(vis, list(#"project", project));
  end;
  block()
    let project = #f;
    while (#t)
      let res = read-from-visualizer(vis); //expect: #"compile" "source"
      if (res[0] == #"open-project")
        project := find-project(res[1]);
        open-project-compiler-database(project, 
                                       warning-callback: callback-handler,
                                       error-handler: callback-handler);
        //canonicalize-project-sources(project, force-parse?: #t);
        vis.dfm-index := -1;
        vis.dfm-report-enabled? := #t;
        //send top-level-definitions
        //(#"source", method-name, compilation-record-id, source)
        vis.dfm-report-enabled? := #f;
      elseif (res[0] == #"compile")
        with-library-context (dylan-library-compilation-context()) //get cc from project
          without-dependency-tracking
            dynamic-bind (*progress-stream*           = #f,  // with-compiler-muzzled
                          *demand-load-library-only?* = #f)
              compile-template(res[1], compiler: curry(visualizing-compiler, vis));
              //actually, find fragment and definition by id and call
              //top-level-convert-using-definition and/or use an interactive-layer
              //and call execute-source
              //be careful: probably need to hack top-level-convert-using-definition
              // to generate dfm even if we have a tight library
            end;
          end;
        end;
      end;
    end;
  exception (e :: <condition>)
    format-out("received exception: %=\n", e);
  end;
end
*/

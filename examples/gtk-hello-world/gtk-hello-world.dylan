library: gtk-hello-world
module: gtk-hello-world
author: Eric Kidd
copyright: public domain, use at own risk

define method main(progname, #rest arguments)
  let (argc, argv) = c-arguments(progname, arguments);
  gtk-hello-world(argc, argv);

  exit(exit-code: 0);
end method main;

define method c-arguments(progname :: <string>, arguments)
 => (<integer>, <c-string-vector>)
  let argc = 1 + arguments.size;
  let argv :: <c-string-vector> =
    make(<c-string-vector>, element-count: argc + 1);
  // XXX - We'd need to delete these if we weren't using
  // a garbage collector which handles the C heap.
  argv[0] := progname;
  for (i from 1 below argc,
       arg in arguments)
    argv[i] := arg;
  end for;
  as(<c-pointer-vector>, argv)[argc] := null-pointer;
  values(argc, argv);
end method c-arguments;

define variable delete :: <function> = callback-method() => (c :: <integer>);
  0;
end;

define variable my-destroy :: <function> = callback-method() => ();
  gtk-main-quit();
end;

define constant *null* :: <machine-pointer> = as(<machine-pointer>, 0);

define method gtk-hello-world(argc, argv) => ()
  let (new-argc, new-argv) = gtk-init(argc, argv);
  let window = gtk-window-new($GTK-WINDOW-TOPLEVEL);
  gtk-container-border-width(window, 10);
  let label = gtk-label-new("Hello, world!");
  gtk-container-add(window, label);
  gtk-signal-connect(window, "delete_event", delete, *null*);
  gtk-signal-connect(window, "destroy", my-destroy, *null*);
  gtk-widget-show(label);
  gtk-widget-show(window);
  gtk-main();
end method gtk-hello-world;

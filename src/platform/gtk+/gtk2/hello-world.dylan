module: hello-world
use-libraries: dylan, common-dylan, io, gtk-2, melange-support, command-processor, debugger
use-modules: common-dylan, system, streams, standard-io, format-out, gtk, melange-support, command-processor, debugger, introspection, extensions

define constant generic-dylan-marshaller = 
    callback-method(stub-closure         :: <raw-pointer>,
                    stub-return-value    :: <raw-pointer>,
                    stub-n-param-values  :: <integer>,
                    stub-param-values    :: <raw-pointer>,
                    stub-invocation-hint :: <raw-pointer>,
                    stub-marshal-data    :: <raw-pointer>) => ();
      format-out("Callback called with %= parameters. Marshal data:%=\n", stub-n-param-values, stub-marshal-data);
      force-output(*standard-output*);
      import-value(<object>, make(<gpointer>, pointer: stub-marshal-data))();
//      run-command-processor();
    end;

define function my-signal-connect(instance :: <GObject>, 
                                  signal :: <byte-string>,
                                  function :: <function>,
                                  #key run-after? :: <boolean>)
//  c-include("gtk/gth.h");
  let closure = g-closure-new-simple(100, //c-expr(int:, "sizeof(struct GClosure)"),
                                     #f);
  g-closure-set-meta-marshal
    (closure, function, 
     make(<GClosureMarshal>, 
          pointer: generic-dylan-marshaller.callback-entry));
  g-signal-connect-closure(instance, 
                           signal, 
                           closure, 
                           if(run-after?) 1 else 0 end)
end function my-signal-connect;

define method hello(#rest args)
  format-out("Hello, World!\n");
  force-output(*standard-output*);
end method hello;

define method delete-event(/*widget :: <GtkWidget>, event :: <GdkEvent>, data*/)
  => (deny-deletion? :: <boolean>)
  format-out ("Delete event occurred\n");
  force-output(*standard-output*);
  #t
end method delete-event;

define method destroy-event(/*widget :: <GtkWidget>, data*/) => ()
  gtk-main-quit ()
end method destroy-event;

begin
  gtk-init(application-name(), application-arguments());
  
  let window = gtk-window-new ($GTK-WINDOW-TOPLEVEL);
  g-signal-connect (window, "delete_event", delete-event);
  g-signal-connect (window, "destroy", destroy-event);
  gtk-container-set-border-width (window, 10);

  let button = gtk-button-new-with-label ("Hello, world!");
  g-signal-connect (button, "clicked", hello);
  g-signal-connect (button, "clicked", 
                     curry(gtk-widget-destroy, window));

  gtk-container-add (window, button);

  gtk-widget-show (button);
  gtk-widget-show (window);

  gtk-main();
end;

module: hello-world
use-libraries: dylan, common-dylan, io, gtk-2
use-modules: common-dylan, streams, standard-io, format-out, gtk

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

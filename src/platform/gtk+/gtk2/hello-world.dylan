module: hello-world
use-libraries: dylan, common-dylan, io, gtk-2, melange-support
use-modules: common-dylan, system, streams, standard-io, format-out, gtk, melange-support

define constant hello = callback-method
 (widget :: <GtkWidget>, data :: <raw-pointer>) => ();
/*  let true-data = import-value(<object>,  as(<gpointer>, data));*/
  format-out ("Hello, world from %=!\n", data);
  force-output(*standard-output*);
end;

define method delete-event(widget :: <GtkWidget>, event :: <GdkEvent>, data)
  => (deny-deletion? :: <boolean>)
  format-out ("Delete event occurred\n");
  #t
end method delete-event;

/*
define method destroy(widget :: <GtkWidget>, data) => ()
  gtk-main-quit ()
end method destroy;
*/
begin
  gtk-init(application-name(), application-arguments());
  
  let window = gtk-window-new ($GTK-WINDOW-TOPLEVEL);
//  g-signal-connect (window, "delete_event", delete-event, #f);
//  g-signal-connect (window, "destroy", destroy, #f);
  gtk-container-set-border-width (window, 10);

  let button = gtk-button-new-with-label ("Hello, world!");
  g-signal-connect (button, "clicked", hello, #f);
//  g-signal-connect-swapped (button, "clicked", gtk-widget-destroy, window);

  gtk-container-add (window, button);

  gtk-widget-show (button);
  gtk-widget-show (window);

  gtk-main();
end;

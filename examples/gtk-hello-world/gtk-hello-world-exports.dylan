library: gtk-hello-world
module: dylan-user

define library gtk-hello-world
  use dylan;
  use melange-support;
  use streams;
  use format;
  use standard-io;
end library;

define module simple-gtk
  use dylan;
  use extensions;
  use melange-support;
  use system;
  export
    gtk-init,
    gtk-window-new,
    gtk-container-border-width,
    gtk-label-new,
    gtk-container-add,
    gtk-widget-show,
    gtk-main,
    gtk-signal-connect,
    gtk-main-quit,
    $GtkWindowType$GTK-WINDOW-TOPLEVEL,
    <gpointer>,
    <c-pointer-vector>,
    <c-string-vector>;
end module simple-gtk;

define module gtk-hello-world
  use dylan;
  use extensions;
  use melange-support;
  use system;
  use streams;
  use format;
  use standard-io;
  
  use simple-gtk;
end module;

module: simple-gtk

define interface
  #include "gtk/gtk.h",
    import: {"gtk_init",
	     "gtk_window_new",
	     "gtk_container_border_width",
	     "gtk_label_new",
	     "gtk_container_add",
	     "gtk_widget_show",
	     "gtk_main",
	     "GTK_WINDOW_TOPLEVEL"},
    equate: {"GtkWidget *" => <gpointer>,
	     "GtkContainer *" => <gpointer>,
	     "char*" => <c-string>,
	     "gchar*" => <c-string>},
    map: {"char*" => <byte-string>,
	  "gchar*" => <byte-string>};
  pointer "char**" => <c-string-vector>,
    superclasses: {<c-vector>};
  function "gtk_init",
    input-output-argument: 1,
    input-output-argument: 2;
end;

// Stupid workaround for Melange bug which tries to assign to error()
// whenever it finds a union memeber which is a float.
define method error-setter(value, format-string, #rest format-args) => ()
  apply(error, format-string, format-args);
end method error-setter;

// Another stupid workaround. Sometimes we need to access mapped types
// as pointers, and Melange doesn't provide any way to do so.
define sealed functional class <c-pointer-vector> (<c-vector>) end;

define sealed domain make (singleton(<c-pointer-vector>));

define constant $pointer-size = 4;

define sealed method pointer-value
    (ptr :: <c-pointer-vector>, #key index = 0)
 => (result :: <statically-typed-pointer>);
  pointer-at(ptr,
	     offset: index * $pointer-size,
	     class: <statically-typed-pointer>);
end method pointer-value;

define sealed method pointer-value-setter
    (value :: <statically-typed-pointer>,
     ptr :: <c-pointer-vector>, #key index = 0)
 => (result :: <statically-typed-pointer>);
  pointer-at(ptr,
	     offset: index * $pointer-size,
	     class: <statically-typed-pointer>) := value;
  value;
end method pointer-value-setter;

define sealed method content-size (value :: subclass(<c-pointer-vector>))
 => (result :: <integer>);
  $pointer-size;
end method content-size;

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
	     "gtk_main_quit",
	     "gtk_signal_connect",
	     "GTK_WINDOW_TOPLEVEL"},
    equate: {"char*" => <c-string>,
	     "gchar*" => <c-string>},
    map: {"char*" => <byte-string>,
	  "gchar*" => <byte-string>,
	  "GtkSignalFunc" => <function>};
  pointer "char**" => <c-string-vector>,
    superclasses: {<c-vector>};
  struct "struct _GtkObject" => <gtk-object>;
  struct "struct _GtkWidget" => <gtk-widget>,
    superclasses: {<gtk-object>};
  pointer "GtkMisc*" => <gtk-misc>,
    superclasses: {<gtk-widget>};
  pointer "GtkLabel*" => <gtk-label>,
    superclasses: {<gtk-misc>};
  pointer "GtkAccelLabel*" => <gtk-accel-label>,
    superclasses: {<gtk-label>};
  pointer "GtkTipsQuery*" => <gtk-tips-query>,
    superclasses: {<gtk-label>};
  pointer "GtkArrow*" => <gtk-arrow>,
    superclasses: {<gtk-misc>};
  pointer "GtkImage*" => <gtk-image>,
    superclasses: {<gtk-misc>};
  pointer "GtkPixmap*" => <gtk-pixmap>,
    superclasses: {<gtk-misc>};
  struct "struct _GtkContainer" => <gtk-container>,
    superclasses: {<gtk-widget>};
  pointer "GtkBin*" => <gtk-bin>,
    superclasses: {<gtk-container>};
  pointer "GtkAlignment*" => <gtk-alignment>,
    superclasses: {<gtk-bin>};
  pointer "GtkFrame*" => <gtk-frame>,
    superclasses: {<gtk-bin>};
  pointer "GtkAspectFrame*" => <gtk-aspect-frame>,
    superclasses: {<gtk-frame>};
  pointer "GtkButton*" => <gtk-button>,
    superclasses: {<gtk-bin>};
  pointer "GtkToggleButton*" => <gtk-toggle-button>,
    superclasses: {<gtk-button>};
  pointer "GtkCheckButton*" => <gtk-check-button>,
    superclasses: {<gtk-toggle-button>};
  pointer "GtkRadioButton*" => <gtk-radio-button>,
    superclasses: {<gtk-check-button>};
  pointer "GtkOptionMenu*" => <gtk-option-menu>,
    superclasses: {<gtk-button>};
  pointer "GtkItem*" => <gtk-item>,
    superclasses: {<gtk-bin>};
  pointer "GtkMenuItem*" => <gtk-menu-item>,
    superclasses: {<gtk-item>};
  pointer "GtkCheckMenuItem*" => <gtk-check-menu-item>,
    superclasses: {<gtk-menu-item>};
  pointer "GtkRadioMenuItem*" => <gtk-radio-menu-item>,
    superclasses: {<gtk-check-menu-item>};
  pointer "GtkTearoffMenuItem*" => <gtk-tearoff-menu-item>,
    superclasses: {<gtk-menu-item>};
  pointer "GtkListItem*" => <gtk-list-item>,
    superclasses: {<gtk-item>};
  pointer "GtkTreeItem*" => <gtk-tree-item>,
    superclasses: {<gtk-item>};
  pointer "GtkWindow*" => <gtk-window>,
    superclasses: {<gtk-bin>};
  pointer "GtkColorSelectionDialog*" => <gtk-color-selection-dialog>,
    superclasses: {<gtk-window>};
  pointer "GtkDialog*" => <gtk-dialog>,
    superclasses: {<gtk-window>};
  pointer "GtkInputDialog*" => <gtk-input-dialog>,
    superclasses: {<gtk-dialog>};
  pointer "GtkDrawWindow*" => <gtk-draw-window>,
    superclasses: {<gtk-window>};
  pointer "GtkFileSelection*" => <gtk-file-selection>,
    superclasses: {<gtk-window>};
  pointer "GtkFontSelectionDialog*" => <gtk-font-selection-dialog>,
    superclasses: {<gtk-window>};
  pointer "GtkPlug*" => <gtk-plug>,
    superclasses: {<gtk-window>};
  pointer "GtkBox*" => <gtk-box>,
    superclasses: {<gtk-container>};
  pointer "GtkButtonBox*" => <gtk-button-box>,
    superclasses: {<gtk-box>};
  pointer "GtkHButtonBox*" => <gtk-h-button-box>,
    superclasses: {<gtk-button-box>};
  pointer "GtkVButtonBox*" => <gtk-v-button-box>,
    superclasses: {<gtk-button-box>};
  pointer "GtkVBox*" => <gtk-v-box>,
    superclasses: {<gtk-box>};
  pointer "GtkColorSelection*" => <gtk-color-selection>,
    superclasses: {<gtk-v-box>};
  pointer "GtkGammaCurve*" => <gtk-gamma-curve>,
    superclasses: {<gtk-v-box>};
  pointer "GtkHBox*" => <gtk-h-box>,
    superclasses: {<gtk-box>};
  pointer "GtkCombo*" => <gtk-combo>,
    superclasses: {<gtk-h-box>};
  pointer "GtkStatusbar*" => <gtk-statusbar>,
    superclasses: {<gtk-h-box>};
  pointer "GtkCList*" => <gtk-c-list>,
    superclasses: {<gtk-container>};
  pointer "GtkCTree*" => <gtk-c-tree>,
    superclasses: {<gtk-c-list>};
  pointer "GtkFixed*" => <gtk-fixed>,
    superclasses: {<gtk-container>};
  pointer "GtkNotebook*" => <gtk-notebook>,
    superclasses: {<gtk-container>};
  pointer "GtkFontSelection*" => <gtk-font-selection>,
    superclasses: {<gtk-notebook>};
  pointer "GtkPaned*" => <gtk-paned>,
    superclasses: {<gtk-container>};
  pointer "GtkHPaned*" => <gtk-h-paned>,
    superclasses: {<gtk-paned>};
  pointer "GtkVPaned*" => <gtk-v-paned>,
    superclasses: {<gtk-paned>};
  pointer "GtkLayout*" => <gtk-layout>,
    superclasses: {<gtk-container>};
  pointer "GtkList*" => <gtk-list>,
    superclasses: {<gtk-container>};
  pointer "GtkMenuShell*" => <gtk-menu-shell>,
    superclasses: {<gtk-container>};
  pointer "GtkMenuBar*" => <gtk-menu-bar>,
    superclasses: {<gtk-menu-shell>};
  pointer "GtkMenu*" => <gtk-menu>,
    superclasses: {<gtk-menu-shell>};
  pointer "GtkPacker*" => <gtk-packer>,
    superclasses: {<gtk-container>};
  pointer "GtkSocket*" => <gtk-socket>,
    superclasses: {<gtk-container>};
  pointer "GtkTable*" => <gtk-table>,
    superclasses: {<gtk-container>};
  pointer "GtkToolbar*" => <gtk-toolbar>,
    superclasses: {<gtk-container>};
  pointer "GtkTree*" => <gtk-tree>,
    superclasses: {<gtk-container>};
  pointer "GtkCalendar*" => <gtk-calendar>,
    superclasses: {<gtk-widget>};
  pointer "GtkDrawingArea*" => <gtk-drawing-area>,
    superclasses: {<gtk-widget>};
  pointer "GtkCurve*" => <gtk-curve>,
    superclasses: {<gtk-drawing-area>};
  pointer "GtkEditable*" => <gtk-editable>,
    superclasses: {<gtk-widget>};
  pointer "GtkEntry*" => <gtk-entry>,
    superclasses: {<gtk-editable>};
  pointer "GtkSpinButton*" => <gtk-spin-button>,
    superclasses: {<gtk-entry>};
  pointer "GtkText*" => <gtk-text>,
    superclasses: {<gtk-editable>};
  pointer "GtkRuler*" => <gtk-ruler>,
    superclasses: {<gtk-widget>};
  pointer "GtkHRuler*" => <gtk-h-ruler>,
    superclasses: {<gtk-ruler>};
  pointer "GtkVRuler*" => <gtk-v-ruler>,
    superclasses: {<gtk-ruler>};
  pointer "GtkRange*" => <gtk-range>,
    superclasses: {<gtk-widget>};
  pointer "GtkScale*" => <gtk-scale>,
    superclasses: {<gtk-range>};
  pointer "GtkHScale*" => <gtk-h-scale>,
    superclasses: {<gtk-scale>};
  pointer "GtkVScale*" => <gtk-v-scale>,
    superclasses: {<gtk-scale>};
  pointer "GtkScrollbar*" => <gtk-scrollbar>,
    superclasses: {<gtk-range>};
  pointer "GtkHScrollbar*" => <gtk-h-scrollbar>,
    superclasses: {<gtk-scrollbar>};
  pointer "GtkVScrollbar*" => <gtk-v-scrollbar>,
    superclasses: {<gtk-scrollbar>};
  pointer "GtkSeparator*" => <gtk-separator>,
    superclasses: {<gtk-widget>};
  pointer "GtkHSeparator*" => <gtk-h-separator>,
    superclasses: {<gtk-separator>};
  pointer "GtkVSeparator*" => <gtk-v-separator>,
    superclasses: {<gtk-separator>};
  pointer "GtkPreview*" => <gtk-preview>,
    superclasses: {<gtk-widget>};
  pointer "GtkProgress*" => <gtk-progress>,
    superclasses: {<gtk-widget>};
  pointer "GtkData*" => <gtk-data>,
    superclasses: {<gtk-object>};
  pointer "GtkAdjustment*" => <gtk-adjustment>,
    superclasses: {<gtk-data>};
  pointer "GtkTooltips*" => <gtk-tooltips>,
    superclasses: {<gtk-data>};
  pointer "GtkItemFactory*" => <gtk-item-factory>,
    superclasses: {<gtk-object>};
  function "gtk_init",
    input-output-argument: 1,
    input-output-argument: 2;
end;

define method export-value(cls == <GtkSignalFunc>, value :: <function>) => (result :: <function-pointer>);
  make(<function-pointer>, pointer: value.callback-entry); 
end method export-value;

define method import-value(cls == <function>, value :: <GtkSignalFunc>) => (result :: <function>);
  error("Is this possible?");
end method import-value;

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

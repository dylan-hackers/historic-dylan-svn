module: simple-gtk

define interface
  #include "gtk/gtk.h",
    name-mapper: minimal-name-mapping,
    define: { "G_INLINE_FUNC" => "extern" },
    import: {"gtk_init",
	     "gtk_window_new",
	     "gtk_container_border_width",
	     "gtk_label_new",
	     "gtk_container_add",
	     "gtk_widget_show",
	     "gtk_main",
	     "gtk_main_quit",
	     "gtk_signal_connect",
	     "GTK_WINDOW_TOPLEVEL",
	     "struct _GtkWindow",
	     "struct _GtkLabel"},
    equate: {"char*" => <c-string>,
	     "gchar*" => <c-string>},
    map: {"char*" => <byte-string>,
	  "gchar*" => <byte-string>,
	  "GtkSignalFunc" => <function>};
  pointer "char**" => <c-string-vector>,
    superclasses: {<c-vector>};
  struct "struct _GtkObjectClass" => <gtk-object-class>,
    rename: {"destroy" => gtk-destroy};
  struct "struct _GtkObject" => <gtk-object>;
  struct "struct _GtkWidget" => <gtk-widget>,
    superclasses: {<gtk-object>};
  struct "struct _GtkMisc" => <gtk-misc>,
    superclasses: {<gtk-widget>};
  struct "struct _GtkLabel" => <gtk-label>,
    superclasses: {<gtk-misc>};
  struct "struct _GtkAccelLabel" => <gtk-accel-label>,
    superclasses: {<gtk-label>};
  struct "struct _GtkTipsQuery" => <gtk-tips-query>,
    superclasses: {<gtk-label>};
  struct "struct _GtkArrow" => <gtk-arrow>,
    superclasses: {<gtk-misc>};
  struct "struct _GtkImage" => <gtk-image>,
    superclasses: {<gtk-misc>};
  struct "struct _GtkPixmap" => <gtk-pixmap>,
    superclasses: {<gtk-misc>};
  struct "struct _GtkContainer" => <gtk-container>,
    superclasses: {<gtk-widget>};
  struct "struct _GtkBin" => <gtk-bin>,
    superclasses: {<gtk-container>};
  struct "struct _GtkAlignment" => <gtk-alignment>,
    superclasses: {<gtk-bin>};
  struct "struct _GtkFrame" => <gtk-frame>,
    superclasses: {<gtk-bin>};
  struct "struct _GtkAspectFrame" => <gtk-aspect-frame>,
    superclasses: {<gtk-frame>};
  struct "struct _GtkButton" => <gtk-button>,
    superclasses: {<gtk-bin>};
  struct "struct _GtkToggleButton" => <gtk-toggle-button>,
    superclasses: {<gtk-button>};
  struct "struct _GtkCheckButton" => <gtk-check-button>,
    superclasses: {<gtk-toggle-button>};
  struct "struct _GtkRadioButton" => <gtk-radio-button>,
    superclasses: {<gtk-check-button>};
  struct "struct _GtkOptionMenu" => <gtk-option-menu>,
    superclasses: {<gtk-button>};
  struct "struct _GtkItem" => <gtk-item>,
    superclasses: {<gtk-bin>};
  struct "struct _GtkMenuItem" => <gtk-menu-item>,
    superclasses: {<gtk-item>};
  struct "struct _GtkCheckMenuItem" => <gtk-check-menu-item>,
    superclasses: {<gtk-menu-item>};
  struct "struct _GtkRadioMenuItem" => <gtk-radio-menu-item>,
    superclasses: {<gtk-check-menu-item>};
  struct "struct _GtkTearoffMenuItem" => <gtk-tearoff-menu-item>,
    superclasses: {<gtk-menu-item>};
  struct "struct _GtkListItem" => <gtk-list-item>,
    superclasses: {<gtk-item>};
  struct "struct _GtkTreeItem" => <gtk-tree-item>,
    superclasses: {<gtk-item>};
  struct "struct _GtkWindow" => <gtk-window>,
    superclasses: {<gtk-bin>};
  struct "struct _GtkColorSelectionDialog" => <gtk-color-selection-dialog>,
    superclasses: {<gtk-window>};
  struct "struct _GtkDialog" => <gtk-dialog>,
    superclasses: {<gtk-window>};
  struct "struct _GtkInputDialog" => <gtk-input-dialog>,
    superclasses: {<gtk-dialog>};
  struct "struct _GtkDrawWindow" => <gtk-draw-window>,
    superclasses: {<gtk-window>};
  struct "struct _GtkFileSelection" => <gtk-file-selection>,
    superclasses: {<gtk-window>};
  struct "struct _GtkFontSelectionDialog" => <gtk-font-selection-dialog>,
    superclasses: {<gtk-window>};
  struct "struct _GtkBox" => <gtk-box>,
    superclasses: {<gtk-container>};
  struct "struct _GtkButtonBox" => <gtk-button-box>,
    superclasses: {<gtk-box>};
  struct "struct _GtkHButtonBox" => <gtk-h-button-box>,
    superclasses: {<gtk-button-box>};
  struct "struct _GtkVButtonBox" => <gtk-v-button-box>,
    superclasses: {<gtk-button-box>};
  struct "struct _GtkVBox" => <gtk-v-box>,
    superclasses: {<gtk-box>};
  struct "struct _GtkColorSelection" => <gtk-color-selection>,
    superclasses: {<gtk-v-box>};
  struct "struct _GtkGammaCurve" => <gtk-gamma-curve>,
    superclasses: {<gtk-v-box>};
  struct "struct _GtkHBox" => <gtk-h-box>,
    superclasses: {<gtk-box>};
  struct "struct _GtkCombo" => <gtk-combo>,
    superclasses: {<gtk-h-box>};
  struct "struct _GtkStatusbar" => <gtk-statusbar>,
    superclasses: {<gtk-h-box>};
  struct "struct _GtkCList" => <gtk-c-list>,
    superclasses: {<gtk-container>};
  struct "struct _GtkCTree" => <gtk-c-tree>,
    superclasses: {<gtk-c-list>};
  struct "struct _GtkFixed" => <gtk-fixed>,
    superclasses: {<gtk-container>};
  struct "struct _GtkNotebook" => <gtk-notebook>,
    superclasses: {<gtk-container>};
  struct "struct _GtkFontSelection" => <gtk-font-selection>,
    superclasses: {<gtk-notebook>};
  struct "struct _GtkPaned" => <gtk-paned>,
    superclasses: {<gtk-container>};
  struct "struct _GtkHPaned" => <gtk-h-paned>,
    superclasses: {<gtk-paned>};
  struct "struct _GtkVPaned" => <gtk-v-paned>,
    superclasses: {<gtk-paned>};
  struct "struct _GtkList" => <gtk-list>,
    superclasses: {<gtk-container>};
  struct "struct _GtkMenuShell" => <gtk-menu-shell>,
    superclasses: {<gtk-container>};
  struct "struct _GtkMenuBar" => <gtk-menu-bar>,
    superclasses: {<gtk-menu-shell>};
  struct "struct _GtkMenu" => <gtk-menu>,
    superclasses: {<gtk-menu-shell>};
  struct "struct _GtkPacker" => <gtk-packer>,
    superclasses: {<gtk-container>};
  struct "struct _GtkTable" => <gtk-table>,
    superclasses: {<gtk-container>};
  struct "struct _GtkToolbar" => <gtk-toolbar>,
    superclasses: {<gtk-container>};
  struct "struct _GtkTree" => <gtk-tree>,
    superclasses: {<gtk-container>};
  struct "struct _GtkCalendar" => <gtk-calendar>,
    superclasses: {<gtk-widget>};
  struct "struct _GtkDrawingArea" => <gtk-drawing-area>,
    superclasses: {<gtk-widget>};
  struct "struct _GtkCurve" => <gtk-curve>,
    superclasses: {<gtk-drawing-area>};
  struct "struct _GtkEditable" => <gtk-editable>,
    superclasses: {<gtk-widget>};
  struct "struct _GtkEntry" => <gtk-entry>,
    superclasses: {<gtk-editable>};
  struct "struct _GtkSpinButton" => <gtk-spin-button>,
    superclasses: {<gtk-entry>};
  struct "struct _GtkText" => <gtk-text>,
    superclasses: {<gtk-editable>};
  struct "struct _GtkRuler" => <gtk-ruler>,
    superclasses: {<gtk-widget>};
  struct "struct _GtkHRuler" => <gtk-h-ruler>,
    superclasses: {<gtk-ruler>};
  struct "struct _GtkVRuler" => <gtk-v-ruler>,
    superclasses: {<gtk-ruler>};
  struct "struct _GtkRange" => <gtk-range>,
    superclasses: {<gtk-widget>};
  struct "struct _GtkScale" => <gtk-scale>,
    superclasses: {<gtk-range>};
  struct "struct _GtkHScale" => <gtk-h-scale>,
    superclasses: {<gtk-scale>};
  struct "struct _GtkVScale" => <gtk-v-scale>,
    superclasses: {<gtk-scale>};
  struct "struct _GtkScrollbar" => <gtk-scrollbar>,
    superclasses: {<gtk-range>};
  struct "struct _GtkHScrollbar" => <gtk-h-scrollbar>,
    superclasses: {<gtk-scrollbar>};
  struct "struct _GtkVScrollbar" => <gtk-v-scrollbar>,
    superclasses: {<gtk-scrollbar>};
  struct "struct _GtkSeparator" => <gtk-separator>,
    superclasses: {<gtk-widget>};
  struct "struct _GtkHSeparator" => <gtk-h-separator>,
    superclasses: {<gtk-separator>};
  struct "struct _GtkVSeparator" => <gtk-v-separator>,
    superclasses: {<gtk-separator>};
  struct "struct _GtkPreview" => <gtk-preview>,
    superclasses: {<gtk-widget>};
  struct "struct _GtkProgress" => <gtk-progress>,
    superclasses: {<gtk-widget>};
  struct "struct _GtkData" => <gtk-data>,
    superclasses: {<gtk-object>};
  struct "struct _GtkAdjustment" => <gtk-adjustment>,
    superclasses: {<gtk-data>};
  struct "struct _GtkTooltips" => <gtk-tooltips>,
    superclasses: {<gtk-data>};
  struct "struct _GtkItemFactory" => <gtk-item-factory>,
    superclasses: {<gtk-object>};
  function "gtk_init",
    input-output-argument: 1,
    input-output-argument: 2;
  // The following is a workaround until we can interface with the GTK+ type system
  function "gtk_window_new",
    map-result: <gtk-window>;
  function "gtk_label_new",
    map-result: <gtk-label>;
end;

define method export-value(cls == <GtkSignalFunc>, value :: <function>) => (result :: <function-pointer>);
  make(<function-pointer>, pointer: value.callback-entry); 
end method export-value;

define method import-value(cls == <function>, value :: <GtkSignalFunc>) => (result :: <function>);
  error("Is this possible?");
end method import-value;

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

Module:    gtk-widgets
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// This file is automatically generated from "gtkmenushell.h"; do not edit.

define C-pointer-type <GtkMenuShell*> => <GtkMenuShell>;
define C-pointer-type <GtkMenuShell**> => <GtkMenuShell*>;
define C-pointer-type <GtkMenuShellClass*> => <GtkMenuShellClass>;
define C-pointer-type <GtkMenuShellClass**> => <GtkMenuShellClass*>;

define C-struct <_GtkMenuShell>
  sealed inline-only slot container-value :: <GtkContainer>;
  sealed inline-only slot children-value :: <GList*>;
  sealed inline-only slot active-menu-item-value :: <GtkWidget*>;
  sealed inline-only slot parent-menu-shell-value :: <GtkWidget*>;
  sealed bitfield slot active-value      :: <guint>,
    width: 1;
  sealed bitfield slot have-grab-value   :: <guint>,
    width: 1;
  sealed bitfield slot have-xgrab-value  :: <guint>,
    width: 1;
  sealed bitfield slot button-value      :: <guint>,
    width: 2;
  sealed bitfield slot ignore-leave-value :: <guint>,
    width: 1;
  sealed bitfield slot menu-flag-value   :: <guint>,
    width: 1;
  sealed bitfield slot ignore-enter-value :: <guint>,
    width: 1;
  sealed inline-only slot activate-time-value :: <guint32>;
  pointer-type-name: <_GtkMenuShell*>;
  c-name: "struct _GtkMenuShell";
end;

define C-struct <_GtkMenuShellClass>
  sealed inline-only slot parent-class-value :: <GtkContainerClass>;
  sealed bitfield slot submenu-placement-value ::
	<guint>,
    width: 1;
  sealed inline-only slot deactivate-value :: <C-function-pointer>;
  sealed inline-only slot selection-done-value :: <C-function-pointer>;
  sealed inline-only slot move-current-value :: <C-function-pointer>;
  sealed inline-only slot activate-current-value :: <C-function-pointer>;
  sealed inline-only slot cancel-value   :: <C-function-pointer>;
  pointer-type-name: <_GtkMenuShellClass*>;
  c-name: "struct _GtkMenuShellClass";
end;

define inline-only C-function gtk-menu-shell-get-type
  result value :: <GtkType>;
  c-name: "gtk_menu_shell_get_type";
end;

define inline-only C-function gtk-menu-shell-append
  parameter menu_shell1 :: <GtkMenuShell*>;
  parameter child2     :: <GtkWidget*>;
  c-name: "gtk_menu_shell_append";
end;

define inline-only C-function gtk-menu-shell-prepend
  parameter menu_shell1 :: <GtkMenuShell*>;
  parameter child2     :: <GtkWidget*>;
  c-name: "gtk_menu_shell_prepend";
end;

define inline-only C-function gtk-menu-shell-insert
  parameter menu_shell1 :: <GtkMenuShell*>;
  parameter child2     :: <GtkWidget*>;
  parameter position3  :: <gint>;
  c-name: "gtk_menu_shell_insert";
end;

define inline-only C-function gtk-menu-shell-deactivate
  parameter menu_shell1 :: <GtkMenuShell*>;
  c-name: "gtk_menu_shell_deactivate";
end;

define inline-only C-function gtk-menu-shell-select-item
  parameter menu_shell1 :: <GtkMenuShell*>;
  parameter menu_item2 :: <GtkWidget*>;
  c-name: "gtk_menu_shell_select_item";
end;

define inline-only C-function gtk-menu-shell-activate-item
  parameter menu_shell1 :: <GtkMenuShell*>;
  parameter menu_item2 :: <GtkWidget*>;
  parameter force_deactivate3 :: <gboolean>;
  c-name: "gtk_menu_shell_activate_item";
end;

define inline constant <GtkMenuShell> = <_GtkMenuShell>;
define inline constant <GtkMenuShellClass> = <_GtkMenuShellClass>;
define sealed domain make (singleton(<_GtkMenuShell*>));
define sealed domain initialize (<_GtkMenuShell*>);
define sealed domain make (singleton(<_GtkMenuShellClass*>));
define sealed domain initialize (<_GtkMenuShellClass*>);

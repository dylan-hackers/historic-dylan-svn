Module:    gtk-common
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// This file is automatically generated from "gtkdata.h"; do not edit.

define C-pointer-type <GtkData*> => <GtkData>;
define C-pointer-type <GtkData**> => <GtkData*>;
define C-pointer-type <GtkDataClass*> => <GtkDataClass>;
define C-pointer-type <GtkDataClass**> => <GtkDataClass*>;

define C-struct <_GtkData>
  sealed inline-only slot object-value   :: <GtkObject>;
  pointer-type-name: <_GtkData*>;
  c-name: "struct _GtkData";
end;

define C-struct <_GtkDataClass>
  sealed inline-only slot parent-class-value :: <GtkObjectClass>;
  sealed inline-only slot disconnect-value :: <C-function-pointer>;
  pointer-type-name: <_GtkDataClass*>;
  c-name: "struct _GtkDataClass";
end;

define inline-only C-function gtk-data-get-type
  result value :: <GtkType>;
  c-name: "gtk_data_get_type";
end;

define inline constant <GtkData> = <_GtkData>;
define inline constant <GtkDataClass> = <_GtkDataClass>;
define sealed domain make (singleton(<_GtkData*>));
define sealed domain initialize (<_GtkData*>);
define sealed domain make (singleton(<_GtkDataClass*>));
define sealed domain initialize (<_GtkDataClass*>);

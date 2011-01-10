Module:       Dylan-User
Synopsis:     DUIM back-end for GTK
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1999-2000 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library gtk-duim
  use dylan;
//use threads;

  use duim-utilities;
  use duim-core;
  use duim-gadget-panes;	//---*** until we've got all native gadgets in

//use C-FFI;
  use Glib;
  use Gdk;
  use Gtk;

  export gtk-duim;
end library gtk-duim;

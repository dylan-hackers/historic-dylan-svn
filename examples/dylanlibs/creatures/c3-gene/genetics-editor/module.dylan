Module:    dylan-user
Synopsis:  A Creatures 2 genetics editor
Author:    Chris Double
Copyright: (C) 1999, Chris Double.  All rights reserved.
License:   See License.txt

define module genetics-editor
  use operating-system;
  use file-system;
  use streams;
  use standard-io;
  use print;
  use format-out;
  use format;
  use duim;
  use threads;
  use table-extensions;
  use machine-integer-user;
  use finalization;
  use common-dylan, exclude: { format-to-string };
  use simple-random;
  use creatures-genes;
  use ui-creatures-genes;
  use gno-file;
  use win32-duim, import: { window-handle };
  use win32-user, import: { SendMessage };
  use rich-text-gadget;

  // Add binding exports here.

end module genetics-editor;

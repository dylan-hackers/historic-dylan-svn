Module:    dylan-user
Synopsis:  A Creatures 2 genetics editor
Author:    Chris Double
Copyright: (C) 1999, Chris Double.  All rights reserved.
License:   See License.txt

define library genetics-editor
  use common-dylan;
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
  use machine-word;
  use creatures-genes;
  use ui-creatures-genes;
  use gno-file;
  use double-rich-text-gadget;

  use win32-duim;
  use win32-user;

  // Add any more module exports here.
  export genetics-editor;
end library genetics-editor;

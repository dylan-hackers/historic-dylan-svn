Module:    dylan-user
Synopsis:  A program to compare Creature gene files.
Author:    Chris Double
Copyright: (C) 1999, Chris Double. All rights reserved.
License:   See License.txt

define module gene-compare
  use common-dylan, exclude: { format-to-string };
  use streams;
  use format;
  use duim;
  use rich-text-gadget;
  use creatures-genes;

  use win32-duim, import: { window-handle };
  use win32-user, import: { SendMessage };
  use win32-rich-edit;
  use c-ffi;

  // Add binding exports here.

end module gene-compare;

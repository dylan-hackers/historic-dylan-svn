Module:    dylan-user
Synopsis:  A program to compare Creature gene files.
Author:    Chris Double
Copyright: (C) 1999, Chris Double. All rights reserved.
License:   See License.txt

define library gene-compare
  use common-dylan;
  use duim;
  use io;
  use double-rich-text-gadget;
  use creatures-genes;

  use win32-duim;
  use win32-user;
  use win32-rich-edit;
  use c-ffi;

  // Add any more module exports here.
  export gene-compare;
end library gene-compare;

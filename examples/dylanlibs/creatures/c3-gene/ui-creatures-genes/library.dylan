Module:    dylan-user
Synopsis:  Duim panes and frames for displays Creatures 2 genes.
Author:    Chris Double
Copyright: (C) 1999, Chris Double.  All rights reserved.
License:   See License.txt

define library ui-creatures-genes
  use common-dylan;
  use operating-system;
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
  use gno-file;

  // Add any more module exports here.
  export ui-creatures-genes;
end library ui-creatures-genes;

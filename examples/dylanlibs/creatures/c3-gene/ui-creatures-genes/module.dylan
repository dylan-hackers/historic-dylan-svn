Module:    dylan-user
Synopsis:  Duim panes and frames for displays Creatures 2 genes.
Author:    Chris Double
Copyright: (C) 1999, Chris Double.  All rights reserved.
License:   See License.txt

define module ui-creatures-genes
  use operating-system;
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
  use gno-file;

  // Add binding exports here.
  export ui-creatures-genes-version,
         make-pane-for-gene,
         <gene-pane>,
         pane-gene,
         pane-genome;
end module ui-creatures-genes;

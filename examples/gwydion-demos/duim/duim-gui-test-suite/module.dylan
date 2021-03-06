Module:       Dylan-User
Author:       Andy Armstrong, Shri Amit
Synopsis:     An interactive test-suite for DUIM objects
Copyright:    Original Code is Copyright (c) 1997-2000 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module duim-gui-test-suite
  use common-dylan, exclude: {format-to-string};
/*
  use dylan-extensions,
    import: { debug-name };
*/
  use format;
  use format-out;
  use random;

  use duim;
  use duim-extended-geometry;
  //--- It would be nice not to need to do this...
  use duim-internals,
    import: { \with-abort-restart,
	      $default-text-style,
	      <basic-gadget>,
	      <oriented-gadget-mixin>,
	      collection-gadget-default-label-key,
	      <basic-frame>,
	      do-command-menu-gadgets,
	      do-copy-area,

              // Cursors
	      // pointer-cursor-override, pointer-cursor-override-setter,

              // Scrolling
	      <scrolling-sheet-mixin>,
	      update-scroll-bars,
	      line-scroll-amount,
	      page-scroll-amount,
	      sheet-scroll-range,
              sheet-visible-range, set-sheet-visible-range };

  // The start up function
  export start-tests
end module duim-gui-test-suite;

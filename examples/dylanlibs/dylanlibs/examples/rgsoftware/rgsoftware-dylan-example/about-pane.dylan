Module:    rgsoftware-dylan-example
Synopsis:  About this program.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define pane <about-pane> ()
  // The helpful text describing this pane
  pane help-pane (pane)
    make(<text-editor>, 
         min-width: 200,
         min-height: 100,
         scroll-bars: #"dynamic",
         read-only?: #t, 
         text:
           "This example was developed by:\n\n"
           "  Chris Double\n"
           "  chris@double.co.nz\n\n"
           "The Dylan developmenvironment used was:\n\n"
           "  Functional Developer\n"
           "  http://www.functionalobjects.com\n\n"
           "It can be compiled using the 30 day free trial of\n"
           "Functional Developer.\n\n"
           "More information about the Dylan programming language\n"
           "can be obtained from:\n\n"
           "  http://www.double.co.nz/dylan\n"
           "  http://www.functionalobjects.com\n"
           "  http://www.gwydiondylan.org");

  // Layout the sub-panes within this pane.
  layout (pane)
    make(<group-box>, 
         label: "About",
         child: pane.help-pane);
end pane <about-pane>;


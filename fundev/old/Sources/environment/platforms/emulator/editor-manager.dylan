Module:    Editor-Manager
Synopsis:  Environment-Editor Interface pulled together
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Initialisation.

// Set up internals in the editor-manager library and module.  In
// Kansas (or after), this should probably be done (or overridden) by
// loading some configuration file. 

define function editor-register-all-factories () => ()
  do(editor-register-factory,
     vector($vi-editor-factory,
            $emacsserver-editor-factory,
            $gnuserv-editor-factory,
            $lispworks-editor-factory,
            $deuce-editor-factory));
end function;

define function editor-init () => ()
  editor-register-all-factories();
  // Default editor for the emulator is "lispworks", for now.
  editor-default-factory-name() := #"deuce";
end function;

// Initialise when this file is loaded, for convenience.
editor-init();

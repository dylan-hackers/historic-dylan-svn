Module:    dylan-user
Synopsis:  A wrapper around RG Software's Neural Net DLL
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

// The library that contains the wrappers around the
// neueral net DLL. Clients must use this library to gain
// access to the Neural Net API.
define library rgsoftware-neural-net
  // Import standard Dylan functionality
  use functional-dylan;

  // Import the functionality to access DLL's
  // and call functions contained within those DLL's.
  use c-ffi;

  // Used for the callback macro.
  use win32-common;

  // Used for GetProcAddress and LoadLibrary Windows API functions
  // to call functions in the DLL.
  use win32-kernel;

  // Imports various routines for handling SafeArray's.
  use win32-automation;

  // Export the modules that client programmers will
  // will use to perform the Neural Net functions.
  export 
    rgsoftware-neural-net, 
    rgsoftware-neural-net-internal;
end library rgsoftware-neural-net;

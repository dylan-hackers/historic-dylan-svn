Module:    dylan-user
Synopsis:  A wrapper around RG Software's Neural Net DLL
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

// The 'internal' module provides the wrappers 
// to the raw DLL API. Users of this library can use
// the 'internal' module if they need to access the
// DLL API at a low level.
define module rgsoftware-neural-net-internal
  // Import standard Dylan functionality
  use functional-dylan;

  // For dynamic-bind and thread variables
  use threads;

  // Import functions for accessing and calling
  // functions inside of a DLL.
  use c-ffi;

  // Used for GetProcAddress and LoadLibrary Windows API functions
  // to call functions in the DLL.
  use win32-kernel, import: { GetProcAddress, LoadLibrary };

  // Used for safe array stuff.
  use win32-automation;

  // Export the raw DLL API so users of this module
  // can access them.
  export
    train-indirect,
    input-relevance-indirect,
    load-network-indirect,
    save-network-indirect,
    predict-indirect,
    stop-processing-indirect,
    $nn50-module,
    $train-address,
    $input-relevance-address,
    $load-network-address,
    $save-network-address,
    $predict-address,
    $stop-processing-address,
    train,
    input-relevance,
    load-network,
    save-network,
    predict,
    stop-processing;
end module rgsoftware-neural-net-internal;

// This module is the higher level client module. 
// Most users of this library will use this module
// as it hides the raw DLL API and provides a more
// Dylan-ish feel to the NN functionality.
define module rgsoftware-neural-net
  // Import standard Dylan functionality
  use functional-dylan;

  // use functionality contained in the internal
  // module to access the DLL.
  use rgsoftware-neural-net-internal, prefix: "internal/";

  // Export the higher level API so users of this module
  // can access them.
  export
    <neural-net>,
    neural-net-array,
    train,
    input-relevance,
    load-network,
    save-network,
    predict,
    stop-processing;
end module rgsoftware-neural-net;

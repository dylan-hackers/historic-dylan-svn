Module:    dylan-user
Synopsis:  Protocol for supporting loading of dynamic Dylan libraries
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define module dynamic-library-protocol
  use functional-dylan;
  use win32-kernel, import: { LoadLibrary, FreeLibrary };

  export
    <dynamic-library>,
    dynamic-library-load-callback,
    dynamic-library-unload-callback,
    dynamic-library-description,
    register-dynamic-library,
    load-dynamic-library,
    unload-dynamic-library,
    dynamic-libraries;
end module dynamic-library-protocol;

Module:    dylan-user
Synopsis:  A protocol for tools that can be used in the dynamic draw program
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define module dynamic-draw-tool
  use functional-dylan;

  export 
    <dynamic-draw-tool>,
    do-tool,
    tool-description,
    register-tool,
    unregister-tool,
    all-tools;
end module dynamic-draw-tool;

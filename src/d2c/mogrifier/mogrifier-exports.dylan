module: dylan-user
author: gabor@mac.com

define library mogrifier
  use dylan;
//  use system;
  use common-dylan;
  use melange-support;
  use io;

  export mogrifier;
end library;

define module mogrifier
  use common-dylan;
  use format-out;
//  use magic;
  use introspection;
//  use system;
  use melange-support;

  export compile-function;
end module;

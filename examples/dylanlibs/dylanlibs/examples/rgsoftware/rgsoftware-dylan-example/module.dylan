Module:    dylan-user
Synopsis:  Dylan version of RG Software's NN50 example
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define module rgsoftware-dylan-example
  // Use standard Dylan functionality
  use functional-dylan;

  // Use stream functionality
  use streams;

  // For format-* functions
  use format;

  // Use the Dylan User Interface Manager
  use duim;

  // and the neural net library
  use rgsoftware-neural-net;
end module rgsoftware-dylan-example;

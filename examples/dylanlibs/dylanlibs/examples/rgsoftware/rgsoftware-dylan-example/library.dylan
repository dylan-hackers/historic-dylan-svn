Module:    dylan-user
Synopsis:  Dylan version of RG Software's NN50 example
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define library rgsoftware-dylan-example
  // Use standard Dylan functionality
  use functional-dylan;

  // Use input/output routines
  use io;

  // Use the Dylan User Interface Manager
  use duim;

  // For various file handling functions
  use system;

  // and the neural net library
  use rgsoftware-neural-net;
end library rgsoftware-dylan-example;

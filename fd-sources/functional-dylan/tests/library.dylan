Module:    dylan-user
Synopsis:  Functional Objects extensions library test suite
Author:	   James Kirsch, Shri Amit, Andy Armstrong
Copyright: Copyright (c) 1996-2001 Functional Objects, Inc. All rights reserved.

define library functional-dylan-test-suite
  use functional-dylan;
  use testworks;
  use testworks-specs;
  use dylan-test-suite;

  export functional-dylan-test-suite;
end library functional-dylan-test-suite;

define module functional-dylan-test-suite
  use functional-dylan;
  use simple-format;
  use simple-random;
  use testworks;
  use testworks-specs;
  use dylan-test-suite;		// to get collection testing

  export functional-dylan-test-suite;
end module functional-dylan-test-suite;

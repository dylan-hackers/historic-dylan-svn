Module:    functional-dylan-test-suite
Synopsis:  Functional Objects extensions library test suite
Author:	   Andy Armstrong
Copyright: Copyright (c) 1997-2001 Functional Objects, Inc. All rights reserved.

/// Constant testing

define functional-extensions constant-test $unsupplied ()
  check-true("unsupplied?($unsupplied)", unsupplied?($unsupplied));
  check-false("supplied?($unsupplied) is false", supplied?($unsupplied));
end constant-test $unsupplied;

define functional-extensions constant-test $unfound ()
  check-true("not-found?($unfound)", not-found?($unfound));
  check-false("found?($unfound) is false", found?($unfound));
end constant-test $unfound;

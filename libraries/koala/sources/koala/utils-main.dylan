Module:    utilities
Synopsis:  Module initialization code
Author:    Carl Gay
Copyright: Copyright (c) 2001 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


//// Testing

define constant $debugging-utils :: <boolean> = #f;

define function test-utilities
    () => ()
  test-resource-pools();
end;


//// Initialization

begin
  when ($debugging-utils)
    test-utilities();
  end;
end;



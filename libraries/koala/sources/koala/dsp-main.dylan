Module:    dsp
Synopsis:  Module initialization code
Author:    Carl Gay
Copyright: Copyright (c) 2001 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


//// Testing

define function test-dsp
    () => ()
  // Nothing yet...
end;


//// Initialization

begin
  register-auto-responder("dsp", auto-register-dylan-server-page);
  when (*debugging-dsp*)
    test-dsp();
  end;
end;



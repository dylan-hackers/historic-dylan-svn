Module:    dsp
Synopsis:  Module initialization code
Author:    Carl Gay
Copyright: Copyright (c) 2001 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


//// Initialization

begin
  register-auto-responder(config, "dsp", auto-register-dylan-server-page);
end;



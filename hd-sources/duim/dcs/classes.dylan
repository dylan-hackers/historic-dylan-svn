Module:       duim-dcs-internals
Synopsis:     DUIM display device contexts
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-1999 Harlequin Group plc.
	      All rights reserved.
License:      Harlequin Library Public License Version 1.0
Dual License: GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Drawing context protocol classes

define protocol-class ink (<object>) end;

define protocol-class color (<ink>) end;

// Subclasses might be <gif-image>, <xbm-image>, and so forth
define protocol-class image (<ink>) end;

define protocol-class palette (<object>) end;

define protocol-class pen (<object>) end;

define protocol-class brush (<object>) end;

define protocol-class text-style (<object>) end;

Module:       duim-extended-geometry-internals
Synopsis:     DUIM extended geometry
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-1999 Harlequin Group plc.
	      All rights reserved.
License:      Harlequin Library Public License Version 1.0
Dual License: GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Extended region protocol classes

define protocol-class polyline (<path>) end;

define protocol-class polygon (<area>) end;

define protocol-class line (<path>) end;

define protocol-class rectangle (<area>) end;

define protocol-class elliptical-arc (<path>) end;

define protocol-class ellipse (<area>) end;

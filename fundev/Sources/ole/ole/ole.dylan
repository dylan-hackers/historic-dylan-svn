Module:    OLE
Synopsis:  Manually coded FFI declarations for a few special cases.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//====================================================
//	basic types
//====================================================

// <HANDLE> is defined in the `win32-common' library

define C-subtype <HOLEMENU> ( <HANDLE> ) end;

define C-pointer-type <LPHGLOBAL> => <HGLOBAL>;
define C-pointer-type <LPLOGPALETTE*> => <LPLOGPALETTE>;


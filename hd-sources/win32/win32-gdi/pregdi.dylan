Module:    Win32-GDI
Synopsis:  Declarations to load first.
Author:    David N. Gray
Copyright: 1997, 1998 Harlequin Group plc.  All rights reserved.

// Need these to be open generics because methods are also
// defined in the "ole-controls" library:
define open-accessor xExt-value;
define open-accessor yExt-value;
define open-accessor dwType-value;

// Also defined in "win32-dialog":
define open-accessor lStructSize-value;

Library:      Win32-kernel
Synopsis:     Win32 API for non-GUI system services in "KERNEL32.DLL"
Copyright:    1996, 1998 Harlequin Group plc.  All rights reserved.
Executable: DxW32KNL
Target-type:  DLL
Base-Address: 0x66900000
Major-version: 2
Minor-version: 0
Compilation-mode: tight
Files:	library
	kernfirst
	winnt
	winbase
	winnls
	kernhack
C-Libraries: advapi32.lib
comment:  kernel32.lib is always included by the Dylan library.

Library:      Win32-Registry
Synopsis:     Win32 API for the System Registry.
Copyright:    1997, 1998 Harlequin Group plc.  All rights reserved.
Executable: DxW32REG
Target-type:  DLL
Base-Address: 0x665C0000
Major-version: 2
Minor-version: 0
Compilation-mode: tight
Files:	library
	regconst
	winreg
	regutil
Comment: We need "advapi32.lib", but it is already included by "win32-kernel".

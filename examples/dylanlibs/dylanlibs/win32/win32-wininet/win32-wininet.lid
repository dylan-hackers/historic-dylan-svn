library:	win32-wininet
files:	library
	module
	win32-wininet
base-address:	0x63FE0000
linker-options:	$(guilflags)
c-libraries:	wininet.lib
major-version:	1
minor-version:	0
library-pack:	0
compilation-mode:	tight
target-type:	dll

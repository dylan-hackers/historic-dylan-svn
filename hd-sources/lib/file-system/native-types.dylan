Module:	    file-system
Author:     Gary Palter
Synopsis:   Types used by the File System library API
Copyright:  1996 The Harlequin Group Limited.  All rights reserved.

define constant <pathname> = type-union(<string>, <locator>);

/// Needs a better name, I think ...
define constant <file-type> = one-of(#"file", #"directory", #"link");

define constant <copy/rename-disposition> = one-of(#"signal", #"replace");

define sealed class <file-system-error> (<error>, <simple-condition>)
end class <file-system-error>;

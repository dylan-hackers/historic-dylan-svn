Module:    CL-internals
Author:    Scott McKay
Copyright: 1995 by Harlequin, Inc.

// 'push!' and 'pop!' are intended to be called only on lists
define macro push!
  { push! (?list:name, ?item:expression) }
    => { ?list := add!(?list, ?item) }
end;

define macro pop!
  { pop! (?list:name) }
    => { begin
           let _result = head(?list);
           ?list := tail(?list);
           _result
	 end }
end;

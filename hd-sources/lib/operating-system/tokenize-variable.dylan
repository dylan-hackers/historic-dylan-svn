module:    operating-system
author:    Roman Budzianowski
copyright: 1997 The Harlequin Group Limited. All rights reserved.

// There doesn't seem to be even one file shared by all platforms

// use $environment-variable-delimiter
define function tokenize-environment-variable(var :: <string>)
  => (strings :: <sequence>);
  let strings = make(<stretchy-vector>);
  let max-pos = size(var);
  let old-pos = 0;
  let pos = 0;
  local method collect-string () => ()
	  unless (pos = old-pos)
	    add!(strings, copy-sequence(var, start: old-pos, end: pos));
	  end;
	  old-pos := pos + 1
	end method collect-string;
  while (pos < max-pos)
    let delimiter? = var[pos] = $environment-variable-delimiter;
    delimiter? & collect-string();
    pos := pos + 1
  end;
  collect-string();
  strings
end function tokenize-environment-variable;

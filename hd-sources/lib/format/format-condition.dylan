Module:    format-internals
Author:    Keith Playford, Andy Armstrong
Synopsis:  Use condition-to-string to display a condition
Copyright: 1996, 1997 The Harlequin Group Limited.  All rights reserved.

define sideways method print-object (c :: <condition>, s :: <stream>) => ()
  let message = condition-to-string(c);
  if (*print-escape?* | ~message)
    printing-object (c, s)
      if (message)
	format(s, ": %s", message)
      end
    end
  else
    write(s, message)
  end
end method print-object;

// eof

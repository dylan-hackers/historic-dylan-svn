Module:    generic-arithmetic-internal
Author:    Gary Palter
copyright: Copyright (c) 1997-2000 Functional Objects, Inc. All rights reserved.

///---*** TEMPORARY KLUDGE!

define macro for
  { for (?header:*) ?fbody:* end }
     => { dylan/for (?header) ?fbody end }
end macro for;


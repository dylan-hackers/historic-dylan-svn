Module:    dylan-user
Synopsis:  Various utility functions and classes related to sequences
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define module sequence-utilities
  use common-dylan;

  export
    sequence-position,
    sequence-position-if,
    sequence-position-if-not,
    split-sequence,
    split-string,
    count-if,
    count-if-not,
    find-between,
    search;    
end module sequence-utilities;

Module: btrack
Author: Carl Gay


// Seems like this should be in dylan or common-dylan.
//
define method as
    (type == <integer>, value :: <string>) => (v :: <integer>)
  string-to-integer(value)
end;



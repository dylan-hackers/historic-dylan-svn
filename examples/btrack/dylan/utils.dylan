Module: btrack
Author: Carl Gay


// Seems like this should be in dylan or common-dylan.
//
define method as
    (type == <integer>, value :: <string>) => (v :: <integer>)
  string-to-integer(value)
end;


define class <btrack-exception> (<simple-error>)
end;

// Thrown when a field in a web form fails to validate.
//
define class <invalid-form-field-exception> (<btrack-exception>)
end;


// I use this a lot, so a shorter name is useful.
//
define constant sformat :: <function> = format-to-string;



Module:    rgsoftware-dylan-example
Synopsis:  General Utility Functions
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

// Split a string by commas
define function split-by-comma(string :: <string>) => (s :: <sequence>)
  let start = 0;
  let count = 0;
  let result = make(<stretchy-vector>);

  for(current-index = position(string, ',', skip: count) then position(string, ',', skip: count),
      until: current-index = #f)
    result := add!(result, copy-sequence(string, start: start, end: current-index));
    count := count + 1;
    start := current-index + 1;
  finally
    result := add!(result, copy-sequence(string, start: start));
  end for;
  result; 
end function split-by-comma;

// Convert a string to a floating point number
define method string-to-float(s :: <string>) => (f :: <float>)
  local method is-digit?(ch :: <character>) => (b :: <boolean>)
    let v = as(<integer>, ch);
    v >= as(<integer>, '0') & v <= as(<integer>, '9');
  end method;
  let lhs = make(<stretchy-vector>);
  let rhs = make(<stretchy-vector>);
  let state = #"start";
  let sign = 1;

  local method process-char(ch :: <character>)
    select(state)
      #"start" =>
        select(ch)
          '-' => 
            begin
              sign := -1;
              state := #"lhs";
            end;
          '+' =>
            begin
              sign := 1;
              state := #"lhs";
            end;
          '.' =>
            begin
              lhs := add!(lhs, '0');
              state := #"rhs";
            end;
          otherwise =>
            begin
              state := #"lhs";
              process-char(ch);
            end;
        end select;
      #"lhs" => 
        case
          is-digit?(ch) => lhs := add!(lhs, ch);
          ch == '.' => state := #"rhs";
          otherwise => error("Invalid floating point value.");
        end case;
      #"rhs" =>
        case
          is-digit?(ch) => rhs := add!(rhs, ch);
          otherwise => error("Invalid floating point value.");
        end case;
      otherwise => error("Invalid state while parsing floating point.");
    end select;
  end method;

  for(ch in s)
    process-char(ch);
  end for;

  let lhs = as(<string>, lhs);
  let rhs = if(empty?(rhs)) "0" else as(<string>, rhs) end;
  (string-to-integer(lhs) * sign) +
    as(<double-float>, string-to-integer(rhs) * sign) /
    (10 ^ rhs.size); 
end method string-to-float;

// Convert a floating point to a string without the Dylan specific formatting.
define method tidy-float-to-string(value :: <float>) => (s :: <string>)
  let s = format-to-string("%d", value);
  let dp = subsequence-position(s, ".");
  let tp = subsequence-position(s, "d") | subsequence-position(s, "s") | s.size;
  let lhs = copy-sequence(s, end: dp);
  let rhs = copy-sequence(s, start: dp + 1, end: tp);
  let shift = if(tp = s.size) 0  else string-to-integer(s, start: tp + 1) end;
  let result = "";
  if(shift <= 0)
    result := concatenate(result, lhs, ".");
    for(n from 0 below abs(shift))
      result := concatenate(result, "0");
    end for;
    result := concatenate(result, rhs);
  else
    result := concatenate(result, lhs);
    if(rhs.size < shift)
      result := concatenate(result, rhs);
      for(n from 0 below shift - rhs.size)
        result := concatenate(result, "0");
      end for;
    else
      error("Error parsing float");
    end if;
  end if;
  result;    
end method tidy-float-to-string;

// Function that displays a dialog box requesting input
// of an integer amount. Taken from the Duim test suite example
// provided by Functional Objects.
define method choose-integer
    (#key title = "Select an integer",
          port = default-port(),
          owner)
 => (integer :: false-or(<integer>))
  with-frame-manager (frame-manager(owner))
    let text-field 
      = make(<text-field>,
             value-type: <integer>,
             value-changing-callback: method (gadget)
                                        let dialog = sheet-frame(gadget);
                                        dialog-exit-enabled?(dialog)
                                          := gadget-value(gadget) ~= #f
                                      end,
	     activate-callback: exit-dialog);
    let dialog
      = make(<dialog-frame>, 
	     title: title, 
	     owner: owner,
	     layout: text-field,
	     input-focus: text-field);
    dialog-exit-enabled?(dialog) := gadget-value(text-field) ~= #f;
    start-dialog(dialog)
      & gadget-value(text-field)
  end
end method choose-integer;



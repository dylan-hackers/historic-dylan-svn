Module:    dylanlibs-utilities
Synopsis:  Various utilities that don't fit into other libraries
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

// Convert a string to a floating point number
define method formatted-string-to-float(s :: <string>) => (f :: <float>)
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
end method formatted-string-to-float;

// Convert a floating point to a string without the Dylan specific formatting.
// Prints to the given number of decimal places.
define method float-to-formatted-string(value :: <float>,
                                        #key 
                                        decimal-places) 
 => (s :: <string>)
  let value = if(decimal-places)
                as(<double-float>, truncate(value * 10 ^ decimal-places)) / 10d0 ^ decimal-places
              else
                value
              end if;
  let s = float-to-string(value);
  let dp = subsequence-position(s, ".");
  let tp = subsequence-position(s, "d") | subsequence-position(s, "s") | s.size;
  let lhs = copy-sequence(s, end: dp);
  let rhs = copy-sequence(s, start: dp + 1, end: tp);
  let shift = if(tp = s.size) 0  else string-to-integer(s, start: tp + 1) end;
  let result = "";
  let temp = concatenate(lhs, rhs);
  let d = lhs.size - 1 + shift;
  if(shift < 0)
    for(n from 0 below abs(shift))
      temp := concatenate("0", temp);
    end for;
    d := 0;
  elseif(shift > 0)
    for(n from 0 below shift)
      temp := concatenate(temp, "0");
    end for;
    d := temp.size;
  end if;
      
  concatenate(copy-sequence(temp, start: 0, end: min(d + 1, temp.size)),
              if(d = temp.size)
                ""
              else 
                "."
              end if,
              if(d = temp.size)
                ""
              else
                copy-sequence(temp, 
                              start: d + 1, 
                              end: if(decimal-places) 
                                     min(d + 1 + decimal-places, temp.size)
                                   else 
                                     temp.size 
                                   end)
              end if);
end method float-to-formatted-string;

// Macros for installing simple abort and restart handlers
//
// Define a handler which will exit the block if an <abort>
// restart is signaled.
define macro with-abort-handler
{ with-abort-handler(?initargs:*)
    ?:body
  end }
 => { block(exit)
        let handler (<abort>,
                     init-arguments: vector(?initargs)) =
            method(condition, next-handler)
              exit();
            end method; 

        ?body
      end }
end macro with-abort-handler;

// Define a handler which will exit the block and restart
// it from the beginning if a <simple-restart> is signaled.
define macro with-restart-block-handler
{ with-restart-block-handler(?initargs:*)
    ?:body
  end }
 => { begin
        let restart? = #t;
        while(restart?)
          block(restart-exit)
            let handler (<simple-restart>,
                         init-arguments: vector(?initargs)) =
                method(condition, next-handler)
                  restart? := #t;
                  restart-exit();                
                end method;
            
            restart? := #f;
            ?body
          end block;
        end while;
      end }
end macro with-restart-block-handler;

// Increment a value by an amount. Equivalent to Common Lisp's incf.
define macro inc!
{ inc!(?value:expression, ?change:expression) }
=> { ?value := ?value + ?change }
{ inc!(?value:expression) }
=> { ?value := ?value + 1 }
end macro inc!;

// Decrement a value by an amount. Equivalent to Common Lisp's decf.
define macro dec!
{ dec!(?value:expression, ?change:expression) }
=> { ?value := ?value - ?change }
{ dec!(?value:expression) }
=> { ?value := ?value - 1 }
end macro dec!;


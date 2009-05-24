Module:    hello-world
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function seq1 (#rest sub-rules) => (rule-parser :: <function>)
  local method foo (stream, context)
          values(if (context) #t end, #f, #t);
        end;
  foo;
end;

define constant program-parser-rule = seq1(parse-ablock);

define function parse-program
 (stream, context)
  program-parser-rule(stream, context);
end function;

define constant ablock-parser-rule = seq1 (parse-ablock);

define function parse-ablock
 (stream, context)
  let (prod, succb, err)
    = ablock-parser-rule (stream, context);
  succb := #f;
  values(#t, #t, #t);
end function;




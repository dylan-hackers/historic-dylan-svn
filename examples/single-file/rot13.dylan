module:         rot13
synopsis:       An implementation of ROT13 in Dylan
author:         Peter Hinely
use-libraries:  dylan, io
use-modules:    dylan, standard-io, streams

/*
  Note: When executing the command, you can use control-d
  to end the input.
*/

define method rot13 (c :: <character>)  => (c :: <character>)
  let a = as(<integer>, c);
  if (a > 96)
    if (a < 110) a := a + 13 elseif (a < 123) a := a - 13 end;
  elseif (a > 64)
    if (a < 78)  a := a + 13 elseif (a < 91)  a := a - 13 end;
  end if;
  as(<character>, a);
end method;

define function main () => ()
  let c = #f;
  while (c := read-element(*standard-input*, on-end-of-stream: #f))
    write-element(*standard-output*, rot13(c));
  end;
end function;

main();

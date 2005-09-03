module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define method exclude (list, symbol) => (sequence)
  let res = make(<stretchy-vector>);
  for (i from 0 below list.size by 2)
    if (list[i] ~= symbol)
      add!(res, list[i]);
      add!(res, list[i + 1]);
    end if;
  end for;
  res;
end method;

define method get-sorted-list (table :: <table>)
  => (list :: <list>)
  let res = make(<list>);
  for (ele in table)
    res := add!(res, ele);
  end;
  sort!(res);
end;

define method regexp-match(big :: <string>, regex :: <string>) => (#rest results);
  let (#rest marks) = regexp-position(big, regex);
  let result = make(<stretchy-vector>);

  if(marks[0])
    for(i from 0 below marks.size by 2)
      if(marks[i] & marks[i + 1])
        result := add!(result, copy-sequence(big, start: marks[i], end: marks[i
+ 1]))
      else
        result := add!(result, #f)
      end
    end
  end;
  apply(values, result)
end;

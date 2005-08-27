module: buddha

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
    res := add(res, ele);
  end;
  sort(res);
end;

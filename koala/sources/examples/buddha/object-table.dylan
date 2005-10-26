module: object-table
author: Hannes Mehnert <hannes@mehnert.org>

define constant $obj-to-id-table = make(<table>);
define constant $id-to-obj-table = make(<string-table>);

define variable *counter* :: <integer> = 0;

define method get-reference (object :: <object>) => (res :: <string>)
  let result = element($obj-to-id-table, object, default: #f);
  if (result)
    result;
  else
    let id = integer-to-string(*counter*);
    *counter* := *counter* + 1;
    $obj-to-id-table[object] := id;
    $id-to-obj-table[id] := object;
    id;
  end;
end;

define method get-object (reference :: singleton(#f))
 =>  (res :: false-or(<object>))
  #f;
end;

define method get-object (reference :: <string>) => (res :: false-or(<object>))
  element($id-to-obj-table, reference, default: #f);
end;

module: object-table
author: Hannes Mehnert <hannes@mehnert.org>

define constant $obj-table = make(<string-table>);

define method get-reference (object :: <object>) => (res :: <string>)
  let address = copy-sequence(format-to-string("%=", address-of(object)),
                              start: 1);
  $obj-table[address] := object;
  address;
end;

define method get-object (reference :: singleton(#f))
 =>  (res :: false-or(<object>))
  #f;
end;

define method get-object (reference :: <string>) => (res :: false-or(<object>))
  element($obj-table, reference, default: #f);
end;
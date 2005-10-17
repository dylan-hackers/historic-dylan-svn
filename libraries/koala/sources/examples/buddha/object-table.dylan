module: object-table
author: Hannes Mehnert <hannes@mehnert.org>

define constant $obj-table = make(<string-table>);

define method get-reference (object :: <object>) => (res :: <string>)
  let address = copy-sequence(format-to-string("%=", address-of(object)),
                              start: 1);
  $obj-table[address] := object;
  address;
end;

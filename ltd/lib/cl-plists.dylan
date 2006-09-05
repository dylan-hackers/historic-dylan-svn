Module:    CL-internals
Author:    Scott McKay, Peter Norvig
Copyright: 1995 by Harlequin, Inc.

// Following additions by Peter Norvig:

define constant $symbol-plists :: <object-table> = make(<object-table>);

define method symbol-plist (symbol)
  $symbol-plists[symbol]
end;

define method symbol-plist-setter (value, symbol)
  $symbol-plists[symbol] := value;
end;

define method symbol-get-property (symbol, indicator)
  get-property(symbol-plist(symbol), indicator)
end;

define method symbol-get-property-setter (value, symbol, indicator)
  put-property!(symbol-plist(symbol), indicator, value)
end;

define method symbol-remove-property (symbol, indicator)
  remove-property!(symbol-plist(symbol), indicator)
end;

module: board



define class <board>(<array>)
  
end;


// store objects line by line

define method aref(board :: <board>, #rest coords :: limited(<integer>, min: 0))
  => object :: <object>;

end;
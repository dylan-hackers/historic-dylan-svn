module: board



define class <board>(<array>)
  slot lines :: limited(<vector>, of: limited(<vector>, of: <object>));
end;


// store objects line by line

define method aref(board :: <board>, #rest coords /* :: limited(<integer>, min: 0) */)
 => object :: <object>;
  let x :: limited(<integer>, min: 0) = coords.first;
  let y :: limited(<integer>, min: 0) = coords.second;
  
  board.lines[y][x];
end;


define function dump-board(board :: <board>)
 => ();
  
end;
module: board

define constant <line> = limited(<vector>, of: <object>);

define class <board>(<array>)
  slot lines :: limited(<vector>, of: <line>);
  keyword rows;
  keyword cols;
end;

define method initialize(<board>, #key rows, cols, #all-keys)
  map-as(limited(<vector>, of: <line>),
         method(ignore) make(<line>, size: cols, fill: $empty-space) end,
         range(below: rows));
end;


// store objects line by line

define method aref(board :: <board>, #rest coords /* :: limited(<integer>, min: 0) */)
 => object :: <object>;
  let x :: limited(<integer>, min: 0) = coords.first;
  let y :: limited(<integer>, min: 0) = coords.second;
  
  board.lines[y][x];
end;

define method aref-setter(obj, board :: <board>, #rest coords /* :: limited(<integer>, min: 0) */)
 => object :: <object>;
  let x :: limited(<integer>, min: 0) = coords.first;
  let y :: limited(<integer>, min: 0) = coords.second;
  
  board.lines[y][x] := obj;
end;

define method as(class == <character>, obj :: <character>)
 => obj :: <byte-character>;
  obj
end;

define generic object-from-character(c :: <character>)
 => obj;

define method object-from-character(c == $empty-char)
 => wall :: <false>;
  #f
end;

define constant <wall> = $wall-char.singleton;

define method object-from-character(c == $wall-char)
 => wall :: <wall>;
  c
end;

define constant <water> = $water-char.singleton;

define method object-from-character(c == $water-char)
 => water :: <water>;
  water
end;

define method object-from-character(c == $base-char)
 => base :: <base>;
  <base>.make
end;

define function send-board(s :: <stream>, board :: <board>)
 => ();
  do(curry(write-line, s),
     map(method (line :: <line>)
           map-as(<string>, curry(as, <character>), line)
         end,
         board.lines));
end;

define function receive-board(s :: <stream>, board :: <board>)
 => ();
//  let line = s.read-line;
let landscape =  #("..@...."
  "......."
  "##.~~~~"
  "...~~~~"
  ".......");

  board.lines
    := map-as(limited(<vector>, of: <line>),
              curry(map-as, <line>, object-from-character),
              landscape);


end;




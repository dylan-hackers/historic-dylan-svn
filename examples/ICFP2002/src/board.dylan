module: board


// Terrain types
define abstract functional class <terrain>(<object>)
end;

define macro terrain-definer
  { define terrain ?:name end }
  =>
  {
    define concrete functional class ?name(<terrain>)
    end;
    
    define sealed domain make(?name.singleton);
    define sealed domain initialize(?name);
    
  }
end;

define terrain <wall> end;
define terrain <water> end;
define terrain <base> end;
define terrain <space> end;


// Board

define constant <line> = limited(<vector>, of: <terrain>);
define constant <coordinate> = limited(<integer>, min: 0);

define concrete class <board>(<array>)
  slot lines :: limited(<vector>, of: <line>);
  slot bots;
  slot packages;
//  keyword rows;
//  keyword cols;
end;

define function width(b :: <board>) => w :: <coordinate>;
  b.lines.first.size
end;

define function height(b :: <board>) => w :: <coordinate>;
  b.lines.size
end;

define method initialize(b :: <board>, #key x, y, #all-keys)
  b.lines :=
  map-as(limited(<vector>, of: <line>),
         method(ignore) make(<line>, size: x, fill: $empty-char) end,
         range(below: y));
end;


// store objects line by line

define method aref(board :: <board>, #rest coords /* :: <coordinate> */)
 => object :: <object>;
  let x :: <coordinate> = coords.first;
  let y :: <coordinate> = coords.second;
  
  board.lines[y][x];
end;

define method aref-setter(obj :: <object>, board :: <board>, #rest coords /* :: <coordinate> */)
 => object :: <object>;
  let x :: <coordinate> = coords.first;
  let y :: <coordinate> = coords.second;
  
  board.lines[y][x] := obj;
end;

define method as(class == <character>, obj :: <character>)
 => obj :: <byte-character>;
  obj
end;

define generic terrain-from-character(c :: <character>)
 => terra :: <terrain>;

define method terrain-from-character(c == $empty-char)
 => empty :: <space>;
  <space>.make
end;

//define constant <wall> = $wall-char.singleton;

define method terrain-from-character(c == $wall-char)
 => wall :: <wall>;
  <wall>.make
end;

//define constant <water> = $water-char.singleton;

define method terrain-from-character(c == $water-char)
 => water :: <water>;
  <water>.make
end;

define method terrain-from-character(c == $base-char)
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
              curry(map-as, <line>, terrain-from-character),
              landscape);


end;




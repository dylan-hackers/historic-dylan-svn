module: board

define class <state> (<object>)
  slot board :: <board>, required-init-keyword: board:;
  slot robots :: <collection> = #(), init-keyword: robots:;
  slot packages :: <collection> = #(), init-keyword: packages:;
end class <state>;

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

define concrete class <board> (<array>)
  slot lines :: limited(<vector>, of: <line>);
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
         method(ignore) make(<line>, size: x, fill: '.') end,
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

define function terrain-from-character(c :: <character>)
 => terra :: <terrain>;
  select(c)
    '.' => <space>.make;
    '#' => <wall>.make;
    '~' => <water>.make;
    '@' => <base>.make;
  end select;
end function terrain-from-character;

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




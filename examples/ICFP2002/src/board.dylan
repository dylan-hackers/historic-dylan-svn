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

/*
define function passable?(b :: <board>, p :: <point>)
 => (passable :: <boolean>);
  let ch = b.lines[p.y][p.x];
  ch == '.' | ch == '@';
end;
*/

define function passable?(b :: <board>, x :: <coordinate>, y :: <coordinate>)
 => (passable :: <boolean>);
  let ch = b[y][x];
  ch == '.' | ch == '@';
end;


// Board

define constant <board> = <array>;

define function width(b :: <board>) => w :: <coordinate>;
  b.first.size
end;

define function height(b :: <board>) => w :: <coordinate>;
  b.size
end;

// store objects line by line

define method add-robot (state :: <state>, robot :: <robot>) => <state>;
  // Add a robot to the <state>'s list of robots. If a robot with the
  // same id is present, replace it. If not, add it.
  //
  let robots* = 
    block(return)
      iterate loop (lst = state.robots)
        if (lst.empty?)
          return(pair(robot, state.robots))
        else
          if (lst.head.id = robot.id)
            pair(robot, lst.tail)
          else
            pair(lst.head, lst.tail.loop)
          end if;
        end if;
      end iterate;
    end block;
  make(<state>, board: state.board, robots: robots*, packages: state.packages);
end method add-robot;

/* Package functions: */
define method add-package (state :: <state>, package :: <package>) => <state>;
  // Add a package to the <state>'s list of robots. If a package with the
  // same id is present, replace it. If not, add it.
  //
  let packages* = 
    block(return)
      iterate loop (lst = state.packages)
        if (lst.empty?)
          return(pair(package, state.packages))
        else
          if (lst.head.id = package.id)
            pair(package, lst.tail)
          else
            pair(lst.head, lst.tail.loop)
          end if;
        end if;
      end iterate;
    end block;
  make(<state>, board: state.board, robots: state.robots, packages: packages*);
end method add-package;

define function packages-at(state :: <state>, loc-x, loc-y)
 => (v :: <vector>);
  let result = 
    choose-by(curry(\=, loc-x), map(x, state.packages), state.packages);

  choose-by(curry(\=, loc-y), map(y, result), result);
end function packages-at;

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
     map(method (line)
           map-as(<string>, curry(as, <character>), line)
         end,
         board));
end;

define function receive-board(s :: <stream>, board :: <board>)
 => ();
//  let line = s.read-line;
let landscape =  #("..@...."
  "......."
  "##.~~~~"
  "...~~~~"
  ".......");

  board
    := map-as(limited(<vector>, of: <array>),
              curry(map-as, <array>, terrain-from-character),
              landscape);


end;



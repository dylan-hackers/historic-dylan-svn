module: ants

define functional class <position> (<object>)
  constant slot x :: <integer> = 0, init-keyword: x:;
  constant slot y :: <integer> = 0, init-keyword: y:;
end class <position>;

define function make-position(x :: <integer>, y :: <integer>)
 => (<position>)
  make(<position>, x: x, y: y);
end function make-position;

define constant <direction> = limited(<integer>, min: 0, max: 5);

define function adjacent-cell(p : <position>, d: <direction>)
 => (<position>)
  select(p)
    0 => make-position(p.x + 1, p.y);
    1 => if(even?(p.y)) 
           make-position(p.x, p.y + 1);
         else
           make-position(p.x + 1, p.y + 1);
         end;
    2 => if(even?(p.y)) 
           make-position(p.x - 1, p.y + 1);
         else
           make-position(p.x, p.y + 1);
         end;
    3 => make-position(p.x - 1, p.y);
    4 => if(even?(p.y)) 
           make-position(p.x - 1, p.y - 1);
         else
           make-position(p.x, p.y - 1);
         end;
    5 => if(even?(p.y)) 
           make-position(p.x, p.y - 1);
         else
           make-position(p.x + 1, p.y - 1);
         end;
  end select;
end function adjacent-cell;

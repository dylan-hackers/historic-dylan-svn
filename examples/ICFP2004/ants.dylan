module: ants

define functional class <position> (<object>)
  constant slot x :: <integer> = 0, init-keyword: x:;
  constant slot y :: <integer> = 0, init-keyword: y:;
end class <position>;

define function make-position(x :: <integer>, y :: <integer>)
 => (p :: <position>)
  make(<position>, x: x, y: y);
end function make-position;

define constant <direction> = limited(<integer>, min: 0, max: 5);

define function adjacent-cell(p : <position>, d: <direction>)
 => (d* :: <position>)
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

define constant <left-or-right> = one-of(#"left", #"right");

define function turn(lr :: <left-or-right>, d :: <direction>)
  => (d* :: <direction>)
  if(lr == #"left")
    modulo(d + 5, 6);
  else
    modulo(d + 1, 6);
  end if;
end function turn;

define constant <sense-direction> = one-of(#"Here", #"Ahead", 
                                           #"LeftAhead", #"RightAhead");

define function sensed-cell(p :: <position>, d :: <direction>, 
                            sd :: <sense-direction>)
 => (p* :: <position>)
  select(sd)
    #"Here"       => p;
    #"Ahead"      => adjacent-cell(p, d);
    #"LeftAhead"  => adjacent-cell(p, turn(#"left", d));
    #"RightAhead" => adjacent-cell(p, turn(#"right", d));
  end select;
end function sensed-cell;

define constant <color> = one-of(#"red", #"black");

define function other-color(c :: <color>) => (c* :: <color>)
  if(c == #"red")
    #"black"
  else
    #"red"
  end if;
end function other-color;

define constant <state> = limited(<integer>, min: 0, max: 9999);

define class <ant> (<object>)
  constant slot id :: <integer>, init-keyword: id:;
  slot state :: <state>;
  slot color :: <color>;
  slot resting :: <integer>;
  slot direction :: <direction>;
  slot has-food :: <boolean>;
end class <ant>;

define class <cell> (<object>)
  slot rocky :: <boolean> = #f;
  slot ant :: false-or(<ant>) = #f;
  slot food :: <integer> = 0, init-keyword: food:;
  slot red-markers :: <vector> = make(<vector>, size: 5);
  slot black-markers :: <vector> = make(<vector>, size: 5);
  slot anthill :: false-or(<color>) = #f, init-keyword: anthill:;
end class <cell>;

define variable *world* :: <array> = make(<array>);

define function cell-at(p :: <position>)
  => (<cell>)
  *world*[p.x, p.y]
end function cell-at;

define function some-ant-is-at(p :: <position>)
 => (b :: <boolean>)
  cell-at(p).ant | #t;
end function some-ant-is-at;

define function ant-at(p :: <position>)
 => (b :: false-or(<ant>))
  cell-at(p).ant;
end function ant-at;

define function ant-at-setter(p :: <position>, a :: false-or(<ant>)) => ()
  cell-at(p).ant := a;
end function ant-at-setter;

define constant set-ant-at = ant-at-setter;

define function clear-ant-at(p :: <position>) => ()
  p.ant-at := #f;
end function clear-ant-at;

define function ant-is-alive(aid :: <integer>)
 => (yesno :: <boolean>)
  member?(aid, ant, test: 
            method(id* :: <integer>, a :: <ant>)
             => (yesno :: <boolean>)
                a.id == id*
            end method);
end function ant-is-alive;


define function find-ant(aid :: <integer>)
  => (p :: false-or(<position>));
  block(return)
    for(xx from 0 below *world*.dimensions[0])
      for(yy from 0 below *world*.dimensions[1])
        let p = make-position(x: xx, y: yy);
        if(some-ant-is-at(p) & ant-at(p).id == aid)
          return(p)
        end if;
      end for;
    end for;
  finally
    #f
  end block;
end function find-ant;

define constant kill-ant-at = clear-ant-at;

define function food-at(p :: <position>)
  => (food :: <integer>)
  ant-at(p).food;
end function food-at;

define function food-at-setter(p :: <position>, f :: <integer>) => ()
  ant-at(p).food := f;
end function food-at-setter;

define constant set-food-at = food-at-setter;

define function anthill-at(p :: <position>, c :: <color>)
  => (yesno :: <boolean>)
  cell-at(p).anthill == c;
end function anthill-at;

define function read-map(s :: <stream>) => ();
  let x-size :: <integer> = string-to-integer(read-line(s));
  let y-size :: <integer> = string-to-integer(read-line(s));
  
  *world* := make(<array>, dimensions: vector(x-size, y-size));

  for(xx from 0 below x-size)
    let line = read-line(s);

    for(yy from 0 below y-size,
        yy* from 
          if(even?(xx)) 0 else 1 end
          by 2)
      let options =
        select(line[yy*])
          '#' => #(rocky:, #t);
          '.' => #();
          '+' => #(anthill:, #"red");
          '-' => #(anthill:, #"black");
          '1' => #(food:, 1);            
          '2' => #(food:, 2);            
          '3' => #(food:, 3);            
          '4' => #(food:, 4);            
          '5' => #(food:, 5);            
          '6' => #(food:, 6);            
          '7' => #(food:, 7);            
          '8' => #(food:, 8);            
          '9' => #(food:, 9);
        end select;
      *world*[xx, yy] := apply(<make>, <cell>, options);
    end for;
  end for;
end function read-map;

define constant <marker> = limited(<integer>, min: 0, max: 5);

define function set-marker-at(p :: <position>, c :: <color>, i :: <marker>)
  => ()
  let cell = cell-at(p);
  if(c == #"black") 
    cell.black-marker[i] := #t;
  else 
    cell.red-marker[i] := #t;
  end;
end function set-marker-at;

define function clear-marker-at(p :: <position>, c :: <color>, i :: <marker>)
  => ()
  let cell = cell-at(p);
  if(c == #"black") 
    cell.black-marker[i] := #f;
  else 
    cell.red-marker[i] := #f;
  end;
end function set-marker-at;

define function check-marker-at(p :: <position>, c :: <color>, i :: <marker>)
  => (present? :: <boolean>)
  let cell = cell-at(p);
  if(c == #"black") 
    cell.black-marker[i]
  else 
    cell.red-marker[i]
  end;
end function set-marker-at;

define function check-any-marker-at(p :: <position>, c :: <color>)
  => (present? :: <boolean>)
  let cell = cell-at(p);
  if(c == #"black") 
    any?(identity, cell.black-marker[i])
  else 
    any?(identity, cell.red-marker[i])
  end;
end function set-marker-at;



module: ants

/*
define /* functional */ class <position> (<object>)
  constant slot x :: <integer> = 0, init-keyword: x:;
  constant slot y :: <integer> = 0, init-keyword: y:;
end class <position>;

define function make-position(x :: <integer>, y :: <integer>)
 => (p :: <position>)
  make(<position>, x: x, y: y);
end function make-position;
*/

define constant <position> = <integer>;

define inline-only function x (p :: <position>) => (x :: <integer>);
  logand(p, #x7F)
end function;

define inline-only function y (p :: <position>) => (y :: <integer>);
  ash(p, -7)
end function;

define inline-only function make-position(x :: <integer>, y :: <integer>)
 => (p :: <position>)
  logior(x, ash(y, 7))
end function make-position;

define constant <direction> = limited(<integer>, min: 0, max: 5);

define function adjacent-cell(p :: <position>, d :: <direction>)
 => (d* :: <position>)
  select(d)
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

define variable *ants* :: <stretchy-object-vector> = make(<stretchy-vector>);

define class <ant> (<object>)
  constant slot id :: <integer>, required-init-keyword: id:;
  slot state :: <state> = 0;
  constant slot color :: <color>, required-init-keyword: color:;
  slot resting :: <integer> = 0;
  slot idle-timer :: <integer> = 0;
  slot direction :: <direction> = 0;
  slot has-food :: <boolean> = #f;
  slot ant-position :: <position>, required-init-keyword: at:;
end class <ant>;

define function idle-ant(a :: <ant>)
/*
  a.idle-timer := a.idle-timer + 1;
  if(a.idle-timer > 300 & modulo(a.idle-timer, 100) == 0)
    format-out("Warning: ant %= has been idle for %= turns, state %=:\n"
                 "%=\n\n",
               a, a.idle-timer, a.state,
               unparse(if(color(a) == #"red")
                         *red-brain*[a.state];
                       else
                         *black-brain*[a.state];
                       end if));
  end if;
*/
end function idle-ant;

define function reset-idle-ant(a :: <ant>)
  a.idle-timer := 0;
end function reset-idle-ant;
                                                  

define class <cell> (<object>)
  constant slot rocky :: <boolean> = #f, init-keyword: rocky:;
  slot ant :: false-or(<ant>) = #f;
  slot food :: <integer> = 0, init-keyword: food:;
  constant slot red-marker :: <simple-object-vector> = make(<vector>, size: 6);
  constant slot black-marker :: <simple-object-vector> = make(<vector>, size: 6);
  constant slot anthill :: false-or(<color>) = #f, init-keyword: anthill:;
end class <cell>;

define function cell-at(p :: <position>)
  => (c :: <cell>)
  *world*.cells[p]
end function cell-at;

define function get-ant(aid :: <integer>)
 => (ant :: false-or(<ant>));
  *ants*[aid];
end function get-ant;


define function is-rocky(p :: <position>)
 => (b :: <boolean>)
  cell-at(p).rocky
end function is-rocky;

define function some-ant-is-at(p :: <position>)
 => (b :: <boolean>)
  (cell-at(p).ant & #t) | #f;
end function some-ant-is-at;

define function ant-at(p :: <position>)
 => (b :: false-or(<ant>))
  cell-at(p).ant;
end function ant-at;

// wrong argument order
define function ant-at-setter(p :: <position>, a :: false-or(<ant>)) => ()
  cell-at(p).ant := a;
  if (a)
    a.ant-position := p;
  end;
end function ant-at-setter;

define constant set-ant-at = ant-at-setter;

define function clear-ant-at(p :: <position>) => ()
  cell-at(p).ant := #f;
end function clear-ant-at;

define function ant-is-alive(aid :: <integer>)
 => (yesno :: <boolean>)
  get-ant(aid) ~== #f;
end function ant-is-alive;


define function find-ant(aid :: <integer>)
  => (p :: false-or(<position>));
  let ant = get-ant(aid);
  block(return)
    unless (ant)
      format-out("expect to find ant %d\n", aid);
      return(#f);
    end;
    if (ant.id ~= aid)
      format-out("expect to find ant %d, but got %d\n", aid, ant.id);
    end;
    ant.ant-position;
  end;
end function find-ant;

define function kill-ant-at(p :: <position>) => ()
  let id = cell-at(p).ant.id;
  *ants*[id] := #f;
  clear-ant-at(p);
end function kill-ant-at;

define function food-at(p :: <position>)
  => (food :: <integer>)
  cell-at(p).food;
end function food-at;

define function food-at-setter(p :: <position>, f :: <integer>) => ()
  cell-at(p).food := f;
end function food-at-setter;

define constant set-food-at = food-at-setter;

define function anthill-at(p :: <position>, c :: <color>)
  => (yesno :: <boolean>)
  cell-at(p).anthill == c;
end function anthill-at;

define function read-map(s :: <stream>) => (result :: <world>);
  let x-size :: <integer> = string-to-integer(read-line(s));
  let y-size :: <integer> = string-to-integer(read-line(s));

  if (x-size > 127)
    format-out("ERROR: X size can't be more than 127\n");
  end;
  if (y-size > 127)
    format-out("ERROR: Y size can't be more than 127\n");
  end;
  
  let result = make(<world>, x: x-size, y: y-size, cells: make(<vector>, size: 16384));
  let ant-count = 0;


  for(yy from 0 below y-size)
    let line = read-line(s);

    for(xx from 0 below x-size,
        xx* from 
          if(even?(yy)) 0 else 1 end
          by 2)
      let options =
        select(line[xx*])
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
      let cell = apply(make, <cell>, options);
      if(cell.anthill)
        // if (*ants*.size < 1)
          let ant = make(<ant>, color: cell.anthill, id: ant-count, at: make-position(xx, yy));
          ant-count := ant-count + 1;
          cell.ant := ant;
          add!(*ants*, ant);
        // end if;
      end if;
      result.cells[make-position(xx, yy)] := cell;
    end for;
  end for;
  result;
end function read-map;

define constant <marker> = <integer>;

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
end function clear-marker-at;

define function check-marker-at(p :: <position>, c :: <color>, i :: <marker>)
  => (present? :: <boolean>)
  let cell = cell-at(p);
  if(c == #"black") 
    cell.black-marker[i]
  else 
    cell.red-marker[i]
  end;
end function check-marker-at;

define function check-any-marker-at(p :: <position>, c :: <color>)
  => (present? :: <boolean>)
  let cell = cell-at(p);
  if(c == #"black") 
    any?(identity, cell.black-marker)
  else 
    any?(identity, cell.red-marker)
  end;
end function check-any-marker-at;


define constant <ant-condition> = one-of(#"friend",
                                         #"foe",
                                         #"friendwithfood",
                                         #"foewithfood",
                                         #"food",
                                         #"rock",
                                         #"marker0",
                                         #"marker1",
                                         #"marker2",
                                         #"marker3",
                                         #"marker4",
                                         #"marker5",
                                         #"foemarker",
                                         #"home",
                                         #"foehome");

define function cell-matches(p :: <position>, cond :: <ant-condition>, 
                             c :: <color>) => (yesno :: <boolean>)
  if(rocky(cell-at(p)))
    if(cond == #"rock") #t else #f end;
  else
    select(cond)
      #"friend" => 
        some-ant-is-at(p) & color(ant-at(p)) == c;
      #"foe" => 
        some-ant-is-at(p) & color(ant-at(p)) ~== c;
      #"friendwithfood" =>
        some-ant-is-at(p) & color(ant-at(p)) == c & has-food(ant-at(p));
      #"foewithfood" =>
        some-ant-is-at(p) & color(ant-at(p)) ~== c & has-food(ant-at(p));
      #"food" =>
        food-at(p) > 0;
      #"rock" =>
        #f;
      #"marker0" =>
        check-marker-at(p, c, 0);
      #"marker1" =>
        check-marker-at(p, c, 1);
      #"marker2" =>
        check-marker-at(p, c, 2);
      #"marker3" =>
        check-marker-at(p, c, 3);
      #"marker4" =>
        check-marker-at(p, c, 4);
      #"marker5" =>
        check-marker-at(p, c, 5);
      #"foemarker" =>
        check-any-marker-at(p, other-color(c));
      #"home" =>
        anthill-at(p, c);
      #"foehome" =>
        anthill-at(p, other-color(c));
    end select;
  end if;
end function cell-matches;

define constant <continuation> = type-union(<integer>, <instruction>, <function>);

define class <instruction> (<object>)
end class <instruction>;

define class <sense> (<instruction>)
  slot sense-direction :: <sense-direction>, required-init-keyword: direction:;
  slot state-true :: <continuation>, required-init-keyword: state-true:;
  slot state-false :: <continuation>, required-init-keyword: state-false:;
  slot cond :: <ant-condition>, required-init-keyword: condition:;
end class <sense>;

define class <mark> (<instruction>)
  slot marker :: <marker>, required-init-keyword: marker:;
  slot state :: <continuation>, required-init-keyword: state:;
end class <mark>;

define class <unmark> (<instruction>)
  slot marker :: <marker>, required-init-keyword: marker:;
  slot state :: <continuation>, required-init-keyword: state:;
end class <unmark>;

define class <pickup> (<instruction>)
  slot state-success :: <continuation>, required-init-keyword: state-success:;
  slot state-failure :: <continuation>, required-init-keyword: state-failure:;
end class <pickup>;

define class <drop> (<instruction>)
  slot state :: <continuation>, required-init-keyword: state:;
end class <drop>;

define class <turn> (<instruction>)
  slot left-or-right :: <left-or-right>, required-init-keyword: left-or-right:;
  slot state :: <continuation>, required-init-keyword: state:;
end class <turn>;

define class <move> (<instruction>)
  slot state-success :: <continuation>, required-init-keyword: state-success:;
  slot state-failure :: <continuation>, required-init-keyword: state-failure:;
end class <move>;

define class <flip> (<instruction>)
  slot probability :: <integer>, required-init-keyword: probability:;
  slot state-success :: <continuation>, required-init-keyword: state-success:;
  slot state-failure :: <continuation>, required-init-keyword: state-failure:;
end class <flip>;

define variable *red-brain* :: <stretchy-object-vector> = make(<stretchy-vector>);
define variable *black-brain* :: <stretchy-object-vector> = make(<stretchy-vector>);

define function get-instruction(c :: <color>, s :: <integer>)
  => (i :: <instruction>)
  let brain = if(c == #"red")
                *red-brain*
              else
                *black-brain*
              end if;
  brain[s];
end function get-instruction;

define function read-state-machine(s :: <stream>)
  => (v :: <vector>)
  let v = make(<stretchy-vector>);

  let line-number = 0;
  while(~stream-at-end?(s))
    let line = read-line(s);
    unless(line = "")
      v[line-number] := parse-instruction(line);
      line-number := line-number + 1;
    end unless;
  end while;
  v
end function read-state-machine;

define function parse-instruction(s :: <byte-string>)
  => (i :: false-or(<instruction>))
  let constituents = split-at-whitespace(s);

  local method sym(x)
          as(<symbol>, constituents[x])
        end method sym;

  local method int(x)
          string-to-integer(constituents[x])
        end method int;

  let opcode = sym(0);

  let insn =
    select(opcode)
      #"sense" => 
        let condition = sym(4);
        if(condition = #"marker")
          condition := as(<symbol>, 
                          concatenate(constituents[4],
                                      constituents[5]));
        end if;
        
        make(<sense>, 
             direction: sym(1),
             state-true: int(2),
             state-false: int(3),
             condition: condition);
      #"mark" => make(<mark>, 
                      marker: int(1),
                      state: int(2));
      #"unmark" => make(<unmark>, 
                        marker: int(1),
                        state: int(2));
      #"pickup" => make(<pickup>, 
                        state-success: int(1),
                        state-failure: int(2));
      #"drop" => make(<drop>,
                      state: int(1));
      #"turn" => make(<turn>,
                      left-or-right: sym(1),
                      state: int(2));
      #"move" => make(<move>,
                      state-success: int(1),
                      state-failure: int(2));
      #"flip" => make(<flip>,
                      probability: int(1),
                      state-success: int(2),
                      state-failure: int(3));
      otherwise => #f;
    end select;
  unless(s = insn.unparse)
    format-out("*** Severe warning! Mismatch between assembly and unassembly! ***\n");
  end unless;
  insn
end function parse-instruction;

define function adjacent-ants(p :: <position>, c :: <color>)
  => (count :: <integer>)
  let n = 0;
  for(d from 0 below 6)
    let cel = adjacent-cell(p, d);
    if(some-ant-is-at(cel) & color(ant-at(cel)) == c)
      n := n + 1;
    end if;
  end for;
  n
end function adjacent-ants;

define function check-for-surrounded-ant-at(p :: <position>)
  if(some-ant-is-at(p))
    let a = ant-at(p);
    if(adjacent-ants(p, other-color(color(a))) >= 5)
      kill-ant-at(p);
      set-food-at(p, food-at(p) + 3 + if(has-food(a)) 1 else 0 end);
    end if;
  end if;
end function check-for-surrounded-ant-at;

define function check-for-surrounded-ants(p :: <position>)
  check-for-surrounded-ant-at(p);
  for(d from 0 below 6)
    check-for-surrounded-ant-at(adjacent-cell(p, d));
  end for;
end function check-for-surrounded-ants;

define function step(aid :: <integer>)
  if(ant-is-alive(aid))
    let p = find-ant(aid);
    let a = ant-at(p);
    idle-ant(a);
    if(resting(a) > 0)
      a.resting := resting(a) - 1;
    else
      let ins = get-instruction(color(a), state(a)); 
      select (ins by instance?)
        <sense> =>
          let p* = sensed-cell(p, direction(a), ins.sense-direction);
          let st = if(cell-matches(p*, ins.cond, color(a)))
                     ins.state-true
                   else
                     ins.state-false
                   end if;
          a.state := st;
        <mark> =>
          set-marker-at(p, color(a), ins.marker);
          a.state := ins.state;
        <unmark> =>
          clear-marker-at(p, color(a), ins.marker);
          a.state := ins.state;
        <pickup> =>
          if(has-food(a) | food-at(p) = 0)
            a.state := ins.state-failure;
          else
            set-food-at(p, food-at(p) - 1);
            a.has-food := #t;
            a.state := ins.state-success;
          end if;
        <drop> =>
          if(has-food(a))
            set-food-at(p, food-at(p) + 1);
            a.has-food := #f;
          end if;
          a.state := ins.state;
        <turn> =>
          a.direction := turn(ins.left-or-right, direction(a));
          a.state := ins.state;
        <move> =>
          let newp = adjacent-cell(p, direction(a));
          if(rocky(cell-at(newp)) | some-ant-is-at(newp))
            a.state := ins.state-failure;
          else
            reset-idle-ant(a);
            clear-ant-at(p);
            set-ant-at(newp, a);
            a.state := ins.state-success;
            a.resting := 14;
            check-for-surrounded-ants(newp);
          end if;
        <flip> =>
          let st = if(randomint(ins.probability) == 0)
                     ins.state-success
                   else
                     ins.state-failure
                   end if;
          a.state := st;
      end select;
    end if;
  end if;
end function step;


////////////////////////////////////////////////////////////////////////////////
// Random numbers

define constant *initial-random-seed* :: <integer> = 12345;
define variable *random-seed* :: <integer> = *initial-random-seed*;

define function randomint(n :: <integer>)
 => (randomint :: <integer>);
  let seed = *random-seed*;
  *random-seed* := seed * 22695477 + 1;
  let x = logand(ash(seed,-16), 16383);
  let (q, r) = truncate/(x, n);
  r;
end randomint;

begin
  // discard the first 4 seeds
  randomint(2);
  randomint(2);
  randomint(2);
  randomint(2);
end;

////////////////////////////////////////////////////////////////////////////////

define function aux-load-world(red-brain :: <string>,
                               black-brain :: <string>,
                               world :: <string>)
 => ()
  with-open-file(brain = red-brain)
    *red-brain* := read-state-machine(brain)
  end with-open-file;
  with-open-file(brain = black-brain)
    *black-brain* := read-state-machine(brain)
  end with-open-file;
  with-open-file(world-stream = world)
    *world* := read-map(world-stream)
  end with-open-file;
end function aux-load-world;

define function load-world()
  apply(aux-load-world, application-arguments());
end function load-world;

define variable *number-of-world-steps* :: <integer> = 0;

define function step-world()
  *number-of-world-steps* := *number-of-world-steps* + 1;
//  format-out("Step: %d\n", *number-of-world-steps*);
  for(i from 0 below *ants*.size)
    step(i);
  end for;
end function step-world;

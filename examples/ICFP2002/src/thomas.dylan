module: client

define constant $ready = #"ready";
define constant $nowhere-to-go = #"nowhere-to-go";
define constant $going-to-base = #"going-to-base";
define constant $going-to-dropoff = #"going-to-dropoff";

define constant <goal> = one-of($ready, $nowhere-to-go, $going-to-base,
                                $going-to-dropoff);

// thomas has the obvious pun on Dylan Thomas. There's also the
// less obvious pun that Dylan Thomas was one of history's great
// drunks. 

define class <thomas> (<robot-agent>)
  slot goal :: <goal>,
    init-value: $ready;
  slot moves-remaining :: false-or(<list>),
    init-value: #f;
end class <thomas>;

define method agent-money (tom :: <thomas>, state :: <state>) => <integer>;
  find-robot(state, tom.agent-id).money
end method agent-money;

define method agent-capacity (tom :: <thomas>, state :: <state>) => <integer>;
  find-robot(state, tom.agent-id).capacity
end method agent-capacity;

define method agent-pos (agent :: <robot-agent>, state :: <state>) => <point>;
  find-robot(state, agent.agent-id).location;
end method agent-pos;

define method agent-packages (agent :: <robot-agent>, state :: <state>)
 => (package-list :: <sequence>)
  find-robot(state, agent.agent-id).inventory;
end method agent-packages;

define method packages-with-dest (packages :: <sequence>, loc :: <point>)
 => (lst :: <sequence>)
  choose(method(p) p.dest = loc end method, packages)
end method packages-with-dest;

define method choose-next-base (tom :: <thomas>, state :: <state>) => ()
  debug("choose-next-base\n");
  let tom-pos = agent-pos(tom, state);
  local
    method tom-closer? (base :: <point>, bot :: <robot>) => <boolean>;
      let tom-path = find-path(tom-pos, base, state.board);
      let bot-path = find-path(bot.location, base, state.board);
      if (tom-path)
        if (bot-path) tom-path.size < bot-path.size else #t end if;
      else
        #f
      end if;
    end method tom-closer?,
    method base-closer? (base-1, base-2)
      size(find-path(tom-pos, base-1, state.board)) <
        size(find-path(tom-pos, base-2, state.board))
    end method base-closer?;
  // 1. Find the bases you have not yet visited.
  let maybe-visit = choose(method (x) ~member?(x, tom.visited-bases) end,
                           state.bases);
  // 2. Now, pick the bases for which you are closer than any other
  //    robot.
  let other-robots = remove(state.robots, find-robot(state, tom.agent-id));
  let good-bases = choose(method (base)
                            every?(method(bot) tom-closer?(base, bot) end,
                                   other-robots)
                          end method,
                          maybe-visit);
  // 3. Sort the bases to find the nearest one.
  let sorted-bases = sort(good-bases, test: base-closer?);
  // 4. Decide on our 
  if (~sorted-bases.empty?)
    tom.goal := $going-to-base;
    tom.moves-remaining := find-path(tom-pos,
                                     sorted-bases.first,
                                     state.board);
  else
    tom.goal := $nowhere-to-go; // there's nowhere to go!
  end if;
end method choose-next-base;

/* use load-packages #####
define method choose-packages (packages :: <sequence>, tom :: <thomas>,
                               state :: <state>)
 => (ps :: <sequence>)
  let sorted-packages = sort(packages,
                             test: method(a, b) a.weight > b.weight end);
  let ps = #();
  let tot = 0;
  block(return)
    for (p in sorted-packages)
      if (tot + p.weight > agent-capacity(tom, state))
        return(ps)
      else
        if (find-path(agent-pos(tom, state), p.dest, state.board))
          ps := add(ps, p);
          tot := tot + p.weight;
        end if;
      end if;
    finally
      ps
    end for;
  end block;
end method choose-packages;
*/

define method punt(id :: <integer>) => <command>;
  make(<pick>, id: id, bid: 1, package-ids: #(13575));
end method punt;

define method generate-next-move* (tom :: <thomas>, state :: <state>)
 => (c :: <command>)
  select (tom.goal)
    $ready =>
      begin
        let ps = agent-packages(tom, state);
        if (ps.empty?)
          choose-next-base(tom, state);
          generate-next-move(tom, state)
        else
          tom.goal := $going-to-dropoff;
          tom.moves-remaining := find-path(agent-pos(tom, state),
                                           ps.first.dest,
                                           state.board);
          generate-next-move(tom, state);
        end if;
      end;
    $going-to-base =>
      begin
        let tom-pos = agent-pos(tom, state);
        if (tom.moves-remaining.empty?) // we are at the base
          tom.visited-bases := add(tom.visited-bases,
                                   state.board[tom-pos.y, tom-pos.x]);
//          let ps = choose-packages(packages-at(state, tom-pos),
//                                   tom,
//                                   state);
          let ps = load-packages(tom, state, compare: method(a, b) a.weight > b.weight end);
          if (ps.empty?) // there are no packages we can pick up.
            choose-next-base(tom, state);
            generate-next-move(tom, state);
          else
            tom.goal := $going-to-dropoff;
            tom.moves-remaining := find-path(tom-pos, ps.first.dest,
                                             state.board);
            make(<pick>, id: tom.agent-id, bid: 1, package-ids: map(id, ps));
          end if;
        else
          let next-point = tom.moves-remaining.head;
          tom.moves-remaining := tom.moves-remaining.tail;
          make(<move>,
               id: tom.agent-id,
               bid: 1,
               direction: points-to-direction(tom-pos, next-point));
        end if;
      end;
    $nowhere-to-go => 
      begin
        // First, try to choose a new base. If that's also a failure, punt
        // for a turn.
        choose-next-base(tom, state);
        if (tom.goal = $nowhere-to-go)
          tom.agent-id.punt
        else
          generate-next-move(tom, state);
        end if;
      end;
    $going-to-dropoff =>
      begin
        if (tom.moves-remaining.empty?) // we are at the destination.
          debug("We are at the drop point!\n");
          let ps = packages-with-dest(agent-packages(tom, state),
                                      agent-pos(tom, state));
          // debug("all packages: %=\n", agent-packages(tom, state));
          debug("packages to drop: %=\n", ps);
          tom.goal := $ready;
          make(<drop>, id: tom.agent-id, bid: 1, package-ids: map(id, ps));
        else
          let next-point = tom.moves-remaining.head;
          tom.moves-remaining := tom.moves-remaining.tail;
          make(<move>,
               id: tom.agent-id,
               bid: 1,
               direction: points-to-direction(agent-pos(tom, state),
                                              next-point));
        end if;
      end;
  end select;
end method generate-next-move*;

define method generate-next-move (tom :: <thomas>, state :: <state>)
 => (c :: <command>)
  let c = generate-next-move*(tom, state);
  debug("generate-next-move: goal %=, cmd %=\n", tom.goal, c);
  c
end method generate-next-move;

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
  slot goal-point :: false-or(<point>),
    init-value: #f;
end class <thomas>;

define method packages-with-dest (packages :: <sequence>, loc :: <point>)
 => (lst :: <sequence>)
  choose(method(p) p.dest = loc end method, packages)
end method packages-with-dest;

define method choose-next-base (tom :: <thomas>, state :: <state>) => ()
  debug("choose-next-base\n");
  let tom-pos = agent-pos(tom, state);
  local
    method tom-closer? (base :: <point>, bot :: <robot>) => <boolean>;
      let tom-len = path-length(tom-pos, base, state.board);
      let bot-len = path-length(bot.location, base, state.board);
      if (tom-len)
        if (bot-len) tom-len < bot-len else #t end if;
      else
        #f
      end if;
    end method tom-closer?,
    method base-closer? (base-1, base-2)
      path-length(tom-pos, base-1, state.board) <
        path-length(tom-pos, base-2, state.board)
    end method base-closer?;
  // 1. Find the bases you have not yet visited.
  let maybe-visit = choose(method (x) ~member?(x, tom.visited-bases) end,
                           state.bases);
  // 2. Now, pick the bases for which you are closer than any other
  //    robot, just use all of them.
  let other-robots = remove(state.robots, agent-robot(tom, state));
  let good-bases =
    begin
      let best-bases = 
        choose(method (base)
                 every?(method(bot) tom-closer?(base, bot) end,
                        other-robots)
               end method,
               maybe-visit);
      if (best-bases.empty?) maybe-visit else best-bases end if;
    end;
  // 3. Sort the bases to find the nearest one.
  let sorted-bases = sort(good-bases, test: base-closer?);
  // 4. Decide on our 
  if (~sorted-bases.empty?)
    // The idea here is a psych out. Everyone probably uses the closest-base
    // heuristic or a variation. To avoid stampeding the same base, if there
    // are multiple viable bases, and there's another bot close to me, I will
    // choose the second-closest base.
    if (sorted-bases.size > 1 & bots-within-n-spaces(tom-pos, other-robots, 3))
      tom.goal := $going-to-base;
      tom.goal-point := sorted-bases.second;
    else
      tom.goal := $going-to-base;
      tom.goal-point := sorted-bases.first;
    end if;
  else
    tom.goal := $nowhere-to-go; // there's nowhere to go!
  end if;
end method choose-next-base;

define function bots-within-n-spaces(self :: <point>,
                                     bots :: <sequence>,
                                     dist :: <integer>)
 => (<boolean>)
  any?(method (bot) distance-cost(self, bot.location) <= dist end, bots)
end function bots-within-n-spaces;

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
          tom.goal-point := closest-point(state,
                                          agent-pos(tom, state),
                                          remove-duplicates(map(dest, ps)));
          generate-next-move(tom, state);
        end if;
      end;
    $going-to-base =>
      begin
        let tom-pos = agent-pos(tom, state);
        let path = find-path(agent-pos(tom, state),
                             tom.goal-point,
                             state.board);
        assert(path ~= #f, "thomas: going to base");
        if (path.empty?)
          // we are at the base.
          tom.visited-bases := add(tom.visited-bases,
                                   state.board[tom-pos.y, tom-pos.x]);
          let ps = load-packages(tom,
                                 state,
                                 compare: method(a, b) a.weight > b.weight end);
          if (ps.empty?) // there are no packages we can pick up.
            choose-next-base(tom, state);
            generate-next-move(tom, state);
          else
            tom.goal := $going-to-dropoff;
            tom.goal-point := ps.first.dest;
            make(<pick>, id: tom.agent-id, bid: 1, package-ids: map(id, ps));
          end if;
        else
          let next-point = path.first;
          make(<move>,
               id: tom.agent-id,
               bid: 1,
               direction: points-to-direction(tom-pos, next-point));
        end if;
      end;
    $nowhere-to-go => 
      begin
        // First, try to choose a new base. If that's also a failure, a)
        // clear visited-bases, and b) punt for a turn.
        choose-next-base(tom, state);
        if (tom.goal = $nowhere-to-go)
          tom.visited-bases := #();
          tom.punt
        else
          generate-next-move(tom, state);
        end if;
      end;
    $going-to-dropoff =>
      begin
        let path = find-path(agent-pos(tom, state),
                             tom.goal-point,
                             state.board);
        assert(path ~= #f, "thomas: going to dropoff");
        if (path.empty?) // we are at the destination.
          debug("We are at the drop point!\n");
          let ps = packages-with-dest(agent-packages(tom, state),
                                      agent-pos(tom, state));
          debug("packages to drop: %=\n", ps);
          tom.goal := $ready;
          make(<drop>, id: tom.agent-id, bid: 1, package-ids: map(id, ps));
        else
          let next-point = path.first;
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

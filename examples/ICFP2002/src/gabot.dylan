module: client

define constant <path> = <point-list>;


// ##################
// ## <strategy>
define abstract class <strategy>(<object>)
  slot strategy-robot :: <robot>;
  slot strategy-agent :: <robot-agent>;
end;

// ## valid?
define generic valid?(s :: <strategy>) => valid :: <boolean>;
// ## safe?
define generic safe?(strategy :: <strategy>, me :: <gabot>, s :: <state>) => safe :: <boolean>;
// ## create-command
define generic create-command(s :: <strategy>) => command :: <command>;
// ## create-terminal-command
define generic create-terminal-command(s :: <strategy>, state :: <state>) => command :: <command>;


// ##################
// ## <gabot>
define class <gabot> (<dumbot>)
  slot decided :: <strategy>.false-or = #f;
end class <gabot>;


// ##################
// ## <path-strategy>
define abstract class <path-strategy>(<strategy>)
  slot approach :: <point>, required-init-keyword: approach:;
  slot strategy-path :: <path>, required-init-keyword: path:;
end;

// ## create-command{<path-strategy>}
define method create-command(s :: <path-strategy>) => command :: <command>;
  let robot = s.strategy-robot;
  let path = s.strategy-path;
  let path = path.head = robot.location
             & path.tail
             | path;
  let path = s.strategy-path := path;
  make(<move>, direction: turn(robot, path), bid: 1, id: robot.id);
end;

// ## safe?{<path-strategy>}
define method safe?(dropping :: <path-strategy>, me :: <gabot>, s :: <state>)
 => safe :: <boolean>;
// TODO: any other robots around?
  let position = find-robot(s, me.agent-id).location;
      
      let nearest-bot = s.robots.first; /// HACK

  distance-cost(position, dropping.approach) < distance-cost(position, nearest-bot.location)
  & distance-cost(position, dropping.approach) < distance-cost(dropping.approach, nearest-bot.location)
 //;/// TODO: all?(s.robots x path piints out-of-reach?)

// #t;
end method safe?;


// ##################
// ## <drop-strategy>
define concrete class <drop-strategy>(<path-strategy>)
end;

define function drop-strategy(drop-path :: <path>)
  make(<drop-strategy>, path: drop-path, approach: drop-path.last);
end;

define method valid?(dropping :: <drop-strategy>) => valid :: <boolean>;
  // did we arrive?
  if (dropping.strategy-path.size < 2)
    debug("arrived!\n"); // TODO: if neihbor robots, bid more...
    #f
  else
  //; TODO: do we still have the package?
    #t;
  end;
end;

// ## create-command{<drop-strategy>}
define method create-terminal-command(s :: <drop-strategy>, state :: <state>) => command :: <command>;
  debug("GB: Dropping in create-terminal-command\n");
  make(<drop>, package-ids: /* map(id, choose() */ #(), bid: 1, id: s.strategy-robot.id);
end;



// ##################
// ## <pick-strategy>
define concrete class <pick-strategy>(<path-strategy>)
end;

define function pick-strategy(pick-path :: <path>)
  make(<pick-strategy>, path: pick-path, approach: pick-path.last);
end;



define generic load-packages (agent :: <robot-agent>,
                              state :: <state>,
                              #key compare :: <function>,
                                   cutoff :: false-or(<path-cost>))
 => ps :: <sequence>;

define method load-packages (agent :: <robot-agent>,
                              state :: <state>,
                              #key compare :: <function>,
                                   cutoff :: false-or(<path-cost>))
 => ps :: <sequence>;

  let pos = agent-pos(agent, state);
    
  let sorted-packages = sort(as(<vector>, packages-at(state, pos)), test: compare);
  let ps = #();
  let tot = 0;
  block(return)
    for (p in sorted-packages)
      if (tot + p.weight > agent-capacity(agent, state))
        return(ps)
      else
        if (find-path(pos, p.dest, state.board, cutoff: cutoff))
          ps := add(ps, p);
          tot := tot + p.weight;
        end if;
      end if;
    finally
      ps
    end for;
  end block;
end;

define function pick-compare(p1 :: <package>, p2 :: <package>)
 => better :: <boolean>;
  p1.weight > p2.weight // for now... TODO
end;

// ## create-command{<pick-strategy>}
define method create-terminal-command(s :: <pick-strategy>, state :: <state>) => command :: <command>;
  debug("GB: Picking in create-terminal-command\n");
  make(<pick>, package-ids: map(id, load-packages(s.strategy-agent, state, compare: pick-compare)), bid: 1, id: s.strategy-robot.id);
end;

define method valid?(picking :: <pick-strategy>) => valid :: <boolean>;
  // did we arrive?
  if (picking.strategy-path.size < 2)
    debug("arrived!\n");
    #f
  else
    #t;
  end;
end;



define generic find-safest(me :: <gabot>, coll :: <sequence>, locator :: <function>, s :: <state>, #key weighting :: <function> = identity)
  => (thing, way :: <path>.false-or);

define method find-safest(me :: <gabot>, coll :: <sequence>, locator :: <function>, s :: <state>, #key weighting :: <function> = identity)
  => (thing, way :: <path>.false-or);

  let position = find-robot(s, me.agent-id).location;

    local find-near-safe-place(best-thing, best-path :: <path>)
         => (better-thing, better-path :: <path>);
         
         let distance = best-path == #()
                        & s.board.height + s.board.width + 1
                        | distance-cost(position, best-path.last);

          block (found)
            for (thing in coll)
              let thing-location = thing.locator;
              let path = find-path(position, thing-location, s.board, cutoff: best-thing & distance);
              debug("find-near-safe-place: thing: %=, path: %=\n", thing, path);
              if (path & ~path.empty?)
                if (~best-thing
                    | distance-cost(position, thing-location) < distance) // # FISHY TODO we should compare paths
                  let (better-thing, nearer-path)
                    = find-near-safe-place(thing, path);
                  found(better-thing, nearer-path)
                end if;
              end if;
            end for;
            values(best-thing, best-path)
          end block;
        end method;
  
  
  find-near-safe-place(#f, #());
end method find-safest;



// overall strategy:

// if we have an already cooked-up strategy, try to follow that if still safe 
// (possibly find a better strategy instead?)
// look for safe destinations where I can drop packets
// look for safe bases to pick up packets, or safe forgotten packets in the space
// look for vulnerable robots and I am not vulnerable then attack
// try escape from attackers
// unless empty and others run to a base, then push them (but not if there is water behind me)

define method generate-next-move(me :: <gabot>, s :: <state>)
  => c :: <command>;

  let bot = find-robot(s, me.agent-id);

block (return)
  local method follow(strategy :: <strategy>)
          me.decided := strategy;
          strategy.strategy-robot := bot;
          strategy.strategy-agent := me;
          strategy.create-command.return;
        end;

  local method finish(strategy :: <strategy>)
          me.decided := #f;
          strategy.strategy-robot := bot;
          create-terminal-command(strategy, s).return;
        end;

debug("check\n");
  me.decided
    & (me.decided.valid? | me.decided.finish)
    & safe?(me.decided, me, s)
    & me.decided.follow;
debug("check1\n");
  let (safe-drop, drop-path) = find-safest(me, choose(method(p :: <package>) debug("examining %=\n",p); p.carrier == bot end, s.packages), location, s, weighting: weight);
debug("check11\n");
  safe-drop & drop-path.drop-strategy.follow;
  
//  find-robot(state, agent).inventory
//  reduce(map(weight, packages), 0, \+)
  
debug("check111\n");
  let (safe-pick, pick-path) = find-safest(me, s.bases, identity, s, weighting: weight /* my payload */);
debug("check1111\n");
  safe-pick & pick-path.pick-strategy.follow;
  
/*  ; not yet
  let safe-vulnerable = find-safest(me, s.robots, location, s, weighting: identity);
  safe-vulnerable & safe-vulnerable.kill-strategy.follow;
  */
  
debug("check11111\n");
  next-method()
end block;
end;
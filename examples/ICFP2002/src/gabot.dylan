module: client

define constant <path> = <point-list>;


// ## <strategy>
define abstract class <strategy>(<object>)
end;

// ## valid?
define generic valid?(s :: <strategy>) => valid :: <boolean>;
// ## safe?
define generic safe?(strategy :: <strategy>, me :: <gabot>, s :: <state>) => safe :: <boolean>;
// ## create-command
define generic create-command(s :: <strategy>) => command :: <command>;


define method safe?(dropping :: <drop-strategy>, me :: <gabot>, s :: <state>)
 => safe :: <boolean>;
// TODO: any other robots around?
  let position = find-robot(s, me.agent-id).location;
      
      let nearest-bot = s.robots.first; /// HACK

  distance-cost(position, dropping.approach) < distance-cost(position, nearest-bot.location)
  & distance-cost(position, dropping.approach) < distance-cost(dropping.approach, nearest-bot.location)
 //;/// TODO: all?(s.robots x path piints out-of-reach?)

// #t;
end method safe?;

// ## <gabot>
define class <gabot> (<dumbot>)
  slot decided :: <strategy>.false-or = #f;
end class <gabot>;


// ## <drop-strategy>
define concrete class <drop-strategy>(<strategy>)
  slot approach :: <point>, required-init-keyword: approach:;
  slot strategy-path :: <path>, required-init-keyword: path:;
end;

define function drop-strategy(drop-path :: <path>)
  make(<drop-strategy>, path: drop-path, approach: drop-path.last);
end;

define method valid?(dropping :: <drop-strategy>) => valid :: <boolean>;
// TODO: do we still have the package?
#t;
end;

// ## create-command{<drop-strategy>}
define method create-command(s :: <strategy>) => command :: <command>;
  make(<move>, bid: 1, direction: $north); // HACK ### FIXME
end;

/*
// ---> Error   : Internal compiler error: Trying to get some values back from a function that doesn't return?

define generic find-safest(me :: <gabot>, coll :: <sequence>, location :: <function>, s :: <state>, #key weighting :: <function> = identity)
  => (thing, way :: <path>.false-or);

define method find-safest(me :: <gabot>, coll :: <sequence>, location :: <function>, s :: <state>, #key weighting :: <function> = identity)
  => (thing, way :: <path>.false-or);

  let position = find-robot(s, me.agent-id).location;

    local find-near-safe-place(best-thing, best-path :: <path>)
         => (better-thing, better-path :: <path>);
         
         let distance = distance-cost(position, best-path.last);

          block (found)
            for (thing in coll)
              let path = find-path(position, thing, s.board, cutoff: best-thing & distance);
              if (path)
                if (~best-thing
                    | distance-cost(position, thing.location) < distance) // # FISHY TODO we should compare paths
                  let (better-thing, nearer-path)
                    = find-near-safe-place(thing, path);
                  found(better-thing, nearer-path)
                end if;
              end if;
            end for;
            values(best-thing, distance)
          end block;
        end method;
  
  
  find-near-safe-place(#f, #());
end method find-safest;
*/

define generic find-safest(me :: <gabot>, coll :: <sequence>, location :: <function>, s :: <state>, #key weighting :: <function> = identity)
  => (thing, way :: <path>.false-or);

define method find-safest(me :: <gabot>, coll :: <sequence>, location :: <function>, s :: <state>, #key weighting :: <function> = identity)
  => (thing, way :: <path>.false-or);

  let position = find-robot(s, me.agent-id).location;

    local find-near-safe-place(best-thing, best-path :: <path>)
         => (better-thing, better-path :: <path>);
         
         let distance = best-path == #() & 1001 | distance-cost(position, best-path.last);

          block (found)
            for (thing in coll)
              let path = find-path(position, thing, s.board, cutoff: best-thing & distance);
              debug("find-near-safe-place: thing: %=, path: %=\n", thing, path);
              if (path)
                if (~best-thing
                    | distance-cost(position, thing.location) < distance) // # FISHY TODO we should compare paths
                  let (better-thing, nearer-path)
                    = values(thing, path); //find-near-safe-place(thing, path); ICE#### HACK!
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

define method generate-next-move(me :: <gabot>, s :: <state>)
  => c :: <command>;


block (return)
  local method follow(strategy :: <strategy>)
          me.decided := strategy;
          strategy.create-command.return;
        end;

  me.decided
    & me.decided.valid?
    & safe?(me.decided, me, s)
    & me.decided.follow;

  let safe-drop = find-safest(me, s.packages, location, s, weighting: weight);
  safe-drop & safe-drop.drop-strategy.follow;
  
//  find-robot(state, agent).inventory
//  reduce(map(weight, packages), 0, \+)
  
/*  ; not yet
  let safe-pick = find-safest(me, s.bases, identity, weighting: weight /* my payload */);
  safe-pick & safe-pick.pick-strategy.follow;
  
  let safe-vulnerable = find-safest(me, s.robots, location, weighting: identity);
  safe-vulnerable & safe-vulnerable.kill-strategy.follow;
  */
  
  next-method()
end block;
end;
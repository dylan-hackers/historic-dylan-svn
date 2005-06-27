module: bruce-robber


define class <bruce-robber> (<robber>)
  slot most-recently-robbed-bank :: false-or(<bank>) = #f;
end class <bruce-robber>;


define method choose-move(robber :: <bruce-robber>, world :: <world>)
  let fw-world = future-world-from-world(world);
  let possible-nodes = map(target, generate-moves(robber.agent-player));

  let best-score :: <integer> = -999999999;
  let best-move = #f;

  for (node in possible-nodes)
    let new-world = future-world(fw-world, robber: next-move(robber.agent-player, node));
    let score = evaluate(new-world);
    if (score > best-score)
      best-score := score;
      best-move := node;
    end;
  end;

  dbg("choosing move with score %d\n", best-score);
  make(<move>, target: best-move, transport: "robber");
end method choose-move;


        
define method choose-random-move(robber :: <bruce-robber>, world :: <world>)
  let possible-locations = generate-moves(robber.agent-player);
  possible-locations[random(possible-locations.size)];
end method choose-random-move;


//////////////////////////////////////////////////////////////////////////////////

define class <future-world> (<object>)
  constant slot fw-number         :: <integer>, required-init-keyword: number:;
  constant slot fw-loot           :: <integer>, required-init-keyword: loot:;
  constant slot fw-banks          :: <vec>, required-init-keyword: banks:;
  //constant slot fw-evidences      :: <vec>, required-init-keyword: evidences:;
  //constant slot fw-smell-distance :: <integer>, required-init-keyword: smell:;
  constant slot fw-cops           :: <vec>, required-init-keyword: cops:;
  constant slot fw-robber         :: <player>, required-init-keyword: robber:;
  constant slot fw-skeleton       :: <world-skeleton>, required-init-keyword: skeleton:;
end class;

lock-down <future-world> end;

// a lighter-weight version of world, for planning

define inline method future-world
    (world :: <future-world>,
     #key cops = world.fw-cops,
     loot = world.fw-loot,
     robber = world.fw-robber,
     banks = world.fw-banks)
  => (world :: <future-world>);
  make(<future-world>,
       number: world.fw-number + 1,
       loot: loot,
       banks: banks,
       //evidences: evidences,
       //smell: smell,
       cops: cops,
       robber: robber,
       skeleton: world.fw-skeleton);
end method;


define function future-world-from-world(world :: <world>)
 => (fw :: <future-world>);
  make(<future-world>,
       number: world.world-number,
       loot: world.world-loot,
       banks: world.world-banks,
       //evidences: evidences,
       //smell: smell,
       cops: world.world-cops,
       robber: world.world-robber,
       skeleton: world.world-skeleton);
end future-world-from-world;

define constant *dead* :: <integer> = -1000000;

define method evaluate(world :: <future-world>)
  => (score :: <integer>);
  dbg("start evaluate\n");
  block (return)
    let my-pos = world.fw-robber.player-location;
    let cop-distances = map(method(cop) distance(cop, my-pos) end, world.fw-cops);

    let min-dist = 9999;
    for (dist in cop-distances)
      dbg("  dist = %d\n", dist);
      if (dist < 2)
        return(*dead*)
      end;
      if (dist < min-dist)
        min-dist := dist;
      end;
    end;

    return(min-dist);
  end;
end evaluate;


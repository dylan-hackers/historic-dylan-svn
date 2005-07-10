module: dirty-cop

define class <dirty-cop> (<stupid-predicting-cop>)
end class;

register-bot(<dirty-cop>);

define method choose-move(agent :: <dirty-cop>, world :: <world>)
  /*
  let move = make(<cop-move>,
                  moves: list(random-player-move(agent.agent-player)));
*/
  let move = next-method();
  if (dirty-cop?(agent, world))
    make(<dirty-cop-move>,
         moves: move.moves);
  else
    move.offer := "turncoat:";
    move;
  end;
end method;

define method make-informs (cop ::<dirty-cop>, world :: <world>) => (object)
  next-method();
  #();
end;

define method make-plan (cop :: <dirty-cop>, world :: <world>) => (plan)
  let target-plans = #();
  cop.all-moves := #();
  for (player in world.world-cops)
    let move = random-player-move(player);
    cop.all-moves := add!(cop.all-moves, move);
    target-plans := add!(target-plans, generate-plan(world,
                                                     player,
                                                     move));
  end for;
  for (p in target-plans)
    dbg("WORLD %s PLAN %s %s %s\n", world.world-number, p.plan-bot, p.plan-location.node-name, p.plan-type);
  end;
  target-plans;
end method;

define method make-vote (cop :: <dirty-cop>, world :: <world>) => (vote)
  local method random-sort (plans)
          if (plans.size = 1)
            plans;
          else
            let random-value = plans[random(plans.size)];
            add(random-sort(remove(plans, random-value)), random-value);
          end if;
        end method;
  let res = concatenate(list(cop.agent-player), 
                        map(tail, random-sort(cop.plan-ranking)));
  dbg("RES MAKE VOTE %=\n", map(method(x) x.player-name end, res));
  res;
end method;

define method make-robber-plan (cop :: <dirty-cop>, world :: <world>)
  let plan = #();
  for (player in world.world-dirty-cops)
    let move = random-player-move(player);
    plan := add(plan, generate-plan(world, player, move));
  end for;
  plan;
end method;

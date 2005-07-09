module: dirty-cop

define class <dirty-cop> (<stupid-predicting-cop>)
end class;

register-bot(<dirty-cop>);

define method choose-move(agent :: <dirty-cop>, world :: <world>)
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
  #();
end;

define method perceive-informs (information, cop ::<dirty-cop>, world :: <world>)
end method;

define method perceive-plans (plan-from-messages, cop :: <dirty-cop>, world :: <world>)
  cop.plan-ranking := make(<stretchy-vector>);
  for (fmp :: <from-message-plan> in plan-from-messages)
    unless (fmp.sender = world.world-skeleton.my-name)
      cop.plan-ranking := add!(cop.plan-ranking, find-player(world, fmp.sender));
    end unless;
  end for;
end method;

define method make-plan (cop :: <dirty-cop>, world :: <world>) => (plan)
  let target-plans = #();
  for (player in world.world-cops)
    let move = random-player-move(player);
    if (player = cop.agent-player)
      cop.my-target-move := move;
    end if;
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
            list(plans[0]);
          else
            let random-value = plans[random(plans.size)];
            add(random-sort(remove(plans, random-value)), random-value);
          end if;
        end method;
  let res = concatenate(list(cop.agent-player), random-sort(cop.plan-ranking));
  dbg("RES MAKE VOTE %=\n", map(method(x) x.player-name end, res));
  res;
end method;


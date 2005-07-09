module: cheating-robber

define class <cheating-robber> (<bruce-robber>)
  slot offers = #();
  slot robber-bribe = #f;
  slot allowed-to-push = #t;
end;

register-bot(<cheating-robber>);

define method choose-move(robber :: <cheating-robber>, world :: <world>)
  let move = next-method();
  dbg("MOVE %= %= %=\n", move.target.node-name,
      move.transport,
      move.bot.player-name);
  dbg("OFFERS %=\n", robber.offers);
  if (robber.robber-bribe)
    dbg("BRIBED\n");
    if (robber.offers.size > 0)
      robber.allowed-to-push := #f;
      move.bribe := concatenate("chc\\\n",
                                apply(concatenate,
                                      map(method(x)
                                              concatenate("chc: ", x.player-name, "\n")
                                          end, robber.offers)),
                                "chc/");
      dbg("CHOOSE-MOVE BRIBEL %=\n", move.bribe);
      
    elseif (robber.allowed-to-push) //push
      let closest-cop = find-closest-cop(robber, world);
      dbg("closest cop %s\n", closest-cop.player-name);
      let targets
        = generate-moves-in-direction(closest-cop,
                                      robber.agent-player.player-location.node-id,
                                      transport-type: closest-cop.player-type,
                                      away: #t);
      dbg("targets: %=\n", targets);
      targets := choose(method(x) x.target ~= closest-cop.player-location end,
                        targets);
      //if empty, move another cop
      move.bribe := concatenate("psh: ", closest-cop.player-name, " ",
                                as(<string>, targets[0].target.node-name));
      dbg("MOVE.BRIBE %=\n", move.bribe);
    else // empty choice
      move.bribe := "chc\\\nchc/";
    end if;
  end if;
  dbg("FIN choose-move\n");
  move;
end method;

define method find-closest-cop (robber :: <robber>,
                                world :: <world>)
  sort(world.world-cops,
       test: method(x, y)
                 distance(x, robber.agent-player.player-location) <
                 distance(y, robber.agent-player.player-location)
             end)[0];
end method;
    
define method perceive-offered-cops (offered-cops,
                                     robber :: <cheating-robber>,
                                     world :: <world>)
  dbg("OFFERED COPS %=\n", offered-cops);
  robber.offers := offered-cops;
end method;

define method make-bribe(robber :: <cheating-robber>, world :: <world>)
  if (world.world-loot >= 50)
    robber.robber-bribe := #t;
    "bribe:";
  else
    robber.robber-bribe := #f;
    "nobribe:";
  end if;
end;


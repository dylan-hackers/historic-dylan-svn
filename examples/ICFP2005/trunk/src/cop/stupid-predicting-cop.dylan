module: stupid-predicting-cop

define class <stupid-predicting-cop> (<predicting-cop>)
  slot all-planned-moves :: <collection> = #();
end class;

register-bot(<stupid-predicting-cop>);

define function choose-move-for-aux(moves, player)
  let moves = choose(method(x)
                         x.bot = player;
                     end, moves);
  if(moves.size > 0)
    moves[0]
  else
    #f
  end
end;

define function choose-move-for(cop, player)
  let plan-move = choose-move-for-aux(cop.all-planned-moves,
                                      player);
  
  if (plan-move)
    dbg("Choosing voted plan move for %s\n", player.player-name);
    plan-move
  else
    dbg("Executing own plan move for %s\n", player.player-name);
    choose-move-for-aux(cop.all-moves, player)
  end if;
end function;
  

define method choose-move(cop :: <stupid-predicting-cop>, world :: <world>)
  let moves = map(curry(choose-move-for, cop),
                  concatenate(list(cop.agent-player),
                              map(taken-bot,
                                  choose(method(x)
                                             x.controller = cop.agent-player
                                         end, world.world-bot-takeover))));
  make(<cop-move>,
       moves: moves,
       accusations: cop.accusations);
end method choose-move;

define method make-plan(cop :: <stupid-predicting-cop>, world :: <world>) => (plan)
  cop.all-moves := #();
  let sorted-nodes
    = copy-sequence(sort(range(size: maximum-node-id()),
                         test: method(x, y)
                                   cop.probability-map[x] > cop.probability-map[y]
                               end), end: 5);

  let players = world.world-cops;
  let sorted-players = make(<stretchy-vector>);

  let transports = #();

  if(world.world-number = 1)
    let trans = list("cop-foot", "cop-car");
    for (i from 0 below players.size)
      transports := add!(transports, trans[modulo(i, trans.size)]);
    end for;
  end if;

  for(target in sorted-nodes)
    let remaining-players 
      = sort(players,
             test: method(x, y)
                       distance(x, target.find-node-by-id) 
                         < distance(y, target.find-node-by-id)
                   end method);
    add!(sorted-players, remaining-players[0]);
    players := remove!(players, remaining-players[0]);
  end for;
  
  let plan = make(<stretchy-vector>);

  let move-suggestions = make(<vector>);

  for (other-cop in sorted-players,
       my-target in sorted-nodes,
       i from 0)

    if(cop.probability-map[my-target] = 0.0s0)
      my-target := sorted-nodes[0];
    end;

    let moves = generate-moves-in-direction
      (other-cop, my-target,
       transport-type: if (world.world-number = 1)
                         transports[i];
                       end);

    moves := sort(moves, test: method(x,y)
                                   cop.probability-map[x.target.node-id] >
                                   cop.probability-map[y.target.node-id]
                               end);
    move-suggestions := add(move-suggestions, moves);

  end for;
  
  let generated-moves = #();
  for (move in move-suggestions,
       player in sorted-players)
    let target-move = block(return)
                        for (genmove in move)
                          for (occupied-moves in generated-moves)
                            if ((genmove.target ~= occupied-moves.target)
                                  & (genmove.transport ~= occupied-moves.transport))
                              return(genmove);
                            end if;
                          end for;
                        end for;
                      end block;
    unless (target-move)
      target-move := move[random(move.size)];
    end unless;
    generated-moves := add!(generated-moves, target-move);
    cop.all-moves := add(cop.all-moves, target-move);
    
    add!(plan, generate-plan(world,
                             player,
                             target-move));
  end for;


/*  for (p in plan)
    dbg("WORLD %s PLAN %s %s %s\n", world.world-number, p.plan-bot, p.plan-location.node-name, p.plan-type);
  end;*/
  plan
end method make-plan;

define method perceive-vote (vote,
                             cop :: <stupid-predicting-cop>,
                             world :: <world>);
  if (vote)
    //we'll just look whether the move is valid and do it, if it is.
    cop.all-planned-moves := map(tail, choose(method(x) 
                                                  x.head = vote
                                              end, cop.planned-moves));
    //dbg("WINNER: %s\n", vote);
  end if;
end method perceive-vote;

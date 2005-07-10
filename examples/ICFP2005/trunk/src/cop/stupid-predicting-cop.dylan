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
  let moves = map(curry(choose-move-for, cop), world.world-my-players);
  make(<cop-move>,
       moves: moves,
       //we are always sure players we control aren't bad players
       accusations: choose(method(x)
                               ~ member?(x, world.world-my-players);
                           end, cop.accusations));
end method choose-move;

define method check-moves (moves, world, cop)
  if (world.world-loot > 0)
    for (false-accusation in world.world-false-accusations)
      if (world.world-number = false-accusation.false-accusation-world + 2)
        dbg("FALSE ACC %= accused %= in world %= (curworld %=)\n",
            false-accusation.accusing-bot.player-name,
            false-accusation.accused-bot.player-name,
            false-accusation.false-accusation-world,
            world.world-number);
        cop.invalid-moves[false-accusation.accused-bot.player-name] := 0; //=0?
        cop.invalid-moves[false-accusation.accusing-bot.player-name] := 0; //=0?
      end if;
    end for;
    for (move in moves)
      let player = find-player(world, move.bot.player-name);
      unless (player.player-location = move.target)
        dbg("PLAYER %s didn't follow the winning plan %= LOC %=\n",
            player.player-name,
            move.target.node-name,
            player.player-location.node-name);
        cop.invalid-moves[player.player-name] :=
          element(cop.invalid-moves, player.player-name, default: 0) + 1;
        if (cop.invalid-moves[player.player-name] > 5)
          cop.accusations := add!(cop.accusations, player);
        end if;
      end unless;
    end for;
  end if;
end method;


define method make-plan(cop :: <stupid-predicting-cop>, world :: <world>) => (plan)
  check-moves(cop.all-planned-moves, world, cop);
  cop.all-moves := make(<stretchy-vector>);
  let sorted-nodes
    = copy-sequence(sort(range(size: maximum-node-id()),
                         test: method(x, y)
                                   cop.probability-map[x] > cop.probability-map[y]
                               end), end: 5);

  let players = world.world-cops;
  let sorted-players = make(<stretchy-vector>);

  let transports = make(<stretchy-vector>);

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
                          if (~member?(genmove, generated-moves))
                            return(genmove);
                          end if;
                        end for;
                      end block;
    unless (target-move)
      target-move := move[random(move.size)];
    end unless;
    generated-moves := add!(generated-moves, target-move);
    cop.all-moves := add(cop.all-moves, target-move);
    
    add!(plan, generate-plan(world, player, target-move));
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

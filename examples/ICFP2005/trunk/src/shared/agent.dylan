module: world

define abstract class <agent> (<object>)
  slot agent-player :: <player>;
  slot wanted-name = "DyBot";
end class <agent>;

define method initialize (agent :: <agent>, #rest rest, #key, #all-keys)
  next-method();
  let name = agent.object-class.class-name;
  agent.wanted-name := copy-sequence(name, start: 1, end: name.size - 1);
end method;

define open abstract class <cop> (<agent>)
  slot initial-transport = "cop-foot";
end class <cop>;

define open abstract class <robber> (<agent>)
end class <robber>;

define open generic choose-move(agent :: <agent>, world :: <world>);

define open generic make-informs(agent :: <agent>, world :: <world>) => (informs);

define method make-informs(agent :: <agent>, world :: <world>) => (informs);
  #()
end method make-informs;

define open generic perceive-informs(informs, agent :: <agent>, world :: <world>);
define method perceive-informs(informs, agent :: <agent>, world :: <world>)
end method;

define open generic make-plan(agent :: <agent>, world :: <world>) => (plan);

define method make-plan(agent :: <agent>, world :: <world>) => (informs);
  #()
end method make-plan;

define open generic perceive-plans(plans, agent :: <agent>, world :: <world>);

define method perceive-plans(plans, agent :: <agent>, world :: <world>);
end method perceive-plans;

define open generic perceive-offered-cops(offered-cops, robber :: <robber>, world :: <world>);
define method perceive-offered-cops(offered-cops, robber :: <robber>, world :: <world>);
end method;


define open generic make-vote(agent :: <agent>, world :: <world>) => (vote);

define method make-vote(robber :: <robber>, world :: <world>) => (vote);
  concatenate(list(world.world-my-player), world.world-dirty-cops);
end method make-vote;

define method make-vote(cop :: <cop>, world :: <world>) => (vote);
  concatenate(list(world.world-my-player), world.world-other-cops);
end method make-vote;

define open generic perceive-vote(vote, agent :: <agent>, world :: <world>);

define method perceive-vote(vote, agent :: <agent>, world :: <world>);
end method perceive-vote;

define open generic make-robber-informs(agent :: <cop>, world :: <world>);
define method make-robber-informs(agent :: <cop>, world :: <world>)
  #();
end;

define open generic perceive-robber-informs(informs, agent :: <cop>, world :: <world>);
define method perceive-robber-informs(informs, agent :: <cop>, world :: <world>)
end;

define open generic make-robber-plan(agent :: <cop>, world :: <world>);
define method make-robber-plan(agent :: <cop>, world :: <world>)
  #()
end method;

define open generic perceive-robber-plans(plans, agent :: <cop>, world :: <world>);
define method perceive-robber-plans(plans, agent :: <cop>, world :: <world>)
end method;

define open generic make-robber-vote(agent :: <cop>, world :: <world>);
define method make-robber-vote(agent :: <cop>, world :: <world>)
  concatenate(list(world.world-robber), world.world-dirty-cops);
end;

define open generic perceive-robber-vote(vote, agent :: <cop>, world :: <world>);
define method perceive-robber-vote(vote, agent :: <cop>, world :: <world>)
end;

define open generic make-bribe(agent :: <robber>, world :: <world>);
define method make-bribe(agent :: <robber>, world :: <world>)
  "nobribe:";
end;


define open generic drive-agent(agent :: <agent>,
                                input-stream :: <stream>,
                                output-stream :: <stream>);

define method drive-agent-aux(agent :: <agent>,
                              input-stream :: <stream>,
                              output-stream :: <stream>,
                              skelet :: <world-skeleton>)
 => (world :: <world>)
  let world = read-world(input-stream, skelet);
  agent.agent-player := world.world-my-player;

  send("inf\\\n");
  block()
    do(print, make-informs(agent, world));
  exception (e :: <condition>)
    dbg("Error %= while make-informs, ignored\n", e);
  end block;
  send("inf/\n");
  block()
    perceive-informs(read-from-message-inform(input-stream),
                     agent, world);
  exception (e :: <condition>)
    dbg("Error %= while perceive-informs, ignored\n", e);
  end block;
  
  send("plan\\\n");
  block()
    do(print, make-plan(agent, world));
  exception (e :: <condition>)
    dbg("Error %= while make-plan, ignored\n", e);
  end block;
  send("plan/\n");
  
  let plans = read-from-message-plan(input-stream);
  block()
    perceive-plans(plans, agent, world);
  exception (e :: <condition>)
    dbg("Error %= while perceive-plans, ignored\n", e);
  end block;
  
  send("vote\\\n");
  block()
    do(method(x) send("vote: %s\n", x.player-name) end,
       make-vote(agent, world));
  exception (e :: <condition>)
    do(method(x) send("vote: %s\n", x) end,
       map(sender, plans));
    dbg("Error %= while make-vote, ignored\n", e);
  end block;
  send("vote/\n");
  
  block()
    perceive-vote(read-vote-tally(input-stream), agent, world);
  exception (e :: <condition>)
    dbg("Error %= while read-vote-tally, ignored\n", e);
  end block;
  world;
end method;

define method drive-agent(agent :: <robber>,
                          input-stream :: <stream>,
                          output-stream :: <stream>)
  block()
    format(output-stream, "reg: %s robber\n", agent.wanted-name);
    force-output(output-stream);
    let skelet = read-world-skeleton(input-stream);
    block()
      while (#t)

        let world = drive-agent-aux(agent, input-stream, output-stream, skelet);

        block()
          send("%s\n", make-bribe(agent, world));
        exception (e :: <condition>)
          send("nobribe:\n");
          dbg("Error %= while make-bribe, ignored\n", e);
        end;

        dbg("OFFERED\n");
        let offered-cops = map(compose(curry(find-player, world),
                                       bot),
                               read-offered-cops(input-stream));
        dbg("cops %=\n", offered-cops);

        block()
          perceive-offered-cops(offered-cops, agent, world);
        exception (e :: <condition>)
          dbg("Error %= while perceive-offered-cops\n", e);
        end block;
        dbg("TRY MOVING\n");
        //dbg("DRIVE-AGENT: %s\n", node-name(choose-move(agent, world)));
        //block()
          print(choose-move(agent, world));
        //exception (e :: <condition>)
        //  print(make(<robber-move>, 
        //             target: agent.agent-player.player-location,
        //             transport: agent.agent-player.player-type,
        //             bot: agent.agent-player)).
        // dbg("Error %= while choose-move, ignored\n", e);
        //end block;
        dbg("FINISHED LOOP\n");
        force-output(output-stream);
      end while;
    exception (condition :: <parse-error>)
    end;
  exception (condition :: <condition>)
    dbg("Robber caught error: %=\n", condition);
    report-condition(condition, *standard-error*);
    dbg("Exiting program\n");
  end;
end method drive-agent;

define method drive-agent(agent :: <cop>,
                          input-stream :: <stream>,
                          output-stream :: <stream>)
  block()
    send("reg: %s %s\n", agent.wanted-name, agent.initial-transport);
    let skelet = read-world-skeleton(*standard-input*);
    
    block()
      while (#t)

        let world = drive-agent-aux(agent, input-stream, output-stream, skelet);

        if (dirty-cop?(agent, world))
          block()
            let move :: <dirty-cop-move> = choose-move(agent, world);
            print(move);
          exception (e :: <condition>)
            print(make(<dirty-cop-move>,
                       moves: list(make(<move>, 
                                        target: agent.agent-player.player-location,
                                        transport: agent.agent-player.player-type,
                                        bot: agent.agent-player))));
            dbg("Error %= while choose-move, ignored\n", e);
          end block;
          
          let world = read-world(*standard-input*, skelet);
          agent.agent-player := world.world-my-player;
        
          send("inf\\\n");
          block()
            do(print, make-robber-informs(agent, world));
          exception (e :: <condition>)
            dbg("Error %= while make-robber-informs, ignored\n", e);
          end block;
          send("inf/\n");
        
          block()
            perceive-robber-informs(read-from-message-inform(input-stream),
                                    agent, world);
          exception (e :: <condition>)
            dbg("Error %= while perceive-robber-informs, ignored\n", e);
          end block;
        
          send("plan\\\n");
          block()
            do(print, make-robber-plan(agent, world));
          exception (e :: <condition>)
            dbg("Error %= while make-robber-plan, ignored\n", e);
          end block;
          send("plan/\n");
          
          block()
            perceive-robber-plans(read-from-message-plan(input-stream),
                                  agent, world);
          exception (e :: <condition>)
            dbg("Error %= while perceive-robber-plans, ignored\n", e);
          end block;
          
          send("vote\\\n");
          block()
            do(method(x) send("vote: %s\n", x.player-name) end,
               make-robber-vote(agent, world));
          exception (e :: <condition>)
            do(method(x) send("vote: %s\n", x.player-name) end,
               concatenate(list(world.world-robber), world.world-dirty-cops));
            dbg("Error %= while make-robber-vote, ignored\n", e);
          end block;
          send("vote/\n");
        
          block()
            perceive-robber-vote(read-vote-tally(input-stream), agent, world);
          exception (e :: <condition>)
            do(method(x) send("vote: %s\n", x) end,
               world.world-skeleton.cop-names);
            dbg("Error %= while read-robber-vote-tally, ignored\n", e);
          end block;
          
        else
          block()
            print(choose-move(agent, world));
          exception (e :: <condition>)
            print(make(<cop-move>,
                       moves: list(make(<move>, 
                                        target: agent.agent-player.player-location,
                                        transport: agent.agent-player.player-type,
                                        bot: agent.agent-player))));
            dbg("Error %= while choose-move, ignored\n", e);
          end block;
        end if;
      end while;
    exception (condition :: <parse-error>)
    end;
  exception (condition :: <condition>)
    dbg("Cop %s caught error: %=\n", agent.agent-player.player-name, condition);
    report-condition(condition, *standard-error*);
    dbg("Exiting program\n");
  end;
end method drive-agent;

define method dirty-cop? (cop, world :: <world>)
  member?(cop.agent-player, world.world-dirty-cops);
end method;

define constant bot-table = make(<case-insensitive-string-table>);
define method register-bot (bot-class)
  bot-table[bot-class.class-name] := bot-class;
end method;

define method find-bot (bot-class)
  bot-table[bot-class];
end method;

define method print (inform :: <inform>)
  if (inform.plan-world < 200) 
    send("inf: %s %s %s %d %d\n", inform.plan-bot,
         inform.plan-location.node-name, inform.plan-type,
         inform.plan-world, inform.inform-certainty);
  end if;
end method print;

define method print (plan :: <plan>)
  if (plan.plan-world < 200) 
    send("plan: %s %s %s %d\n", plan.plan-bot,
         plan.plan-location.node-name,
         plan.plan-type, plan.plan-world);
  end if;
end method print;

    
define class <move> (<object>)
  slot target :: <node>, init-keyword: target:;
  slot transport :: <string>, init-keyword: transport:;
  slot bot :: <player>, init-keyword: bot:;
end class;

lock-down <move>  end;


define method print (move :: <move>)
  send("mov: %s %s %s\n",
       move.target.node-name,
       move.transport,
       move.bot.player-name);
end method;

define class <robber-move> (<move>)
  slot bribe = "nobribe:", init-keyword: bribe:;
end class;

define method print (move :: <robber-move>)
  send("rmov\\\n");
  next-method();
  send("%s\n", move.bribe);
  send("rmov/\n");
end method;

define class <cop-move> (<object>)
  slot moves, init-keyword: moves:;
  slot offer = "straight-arrow:", init-keyword: offer:;
  slot accusations = #(), init-keyword: accusations:;
end class;

define class <dirty-cop-move> (<cop-move>)
end class;

define method print(move :: <cop-move>)
  send("cmov\\\n");
  send("%s\n", move.offer);
  send("mov\\\n");
  do(print, move.moves);
  send("mov/\n");
  send("acc\\\n");
  do(method(x)
         send("acc: %s\n", x.player-name);
     end, move.accusations);
  send("acc/\n");
  send("cmov/\n");
end;

define method print(move :: <dirty-cop-move>)
  send("mov\\\n");
  do(print, move.moves);
  send("mov/\n");
end method;

define method generate-moves-in-direction (player :: <player>,
                                           target-id :: <integer>,
                                           #key transport-type,
                                           away)
 => (moves)
  let moves = if (transport-type)
                generate-moves(make(<move>,
                                    target: player.player-location,
                                    transport: transport-type,
                                    bot: player),
                               keep-current-transport: #t)
              else
                generate-moves(player);
              end if;
  /*for (m in moves)
    dbg("POSSI MOVE: %s %s %s\n",
        player.player-name,
        m.target.node-name,
        m.transport);
  end for;
*/
  let move-distance = make(<vector>, size: moves.size);
  for (i from 0 below moves.size)
    move-distance[i] := distance(player,
                                 find-node-by-id(target-id),
                                 source: moves[i]);
  end for;
  let move-indices
    = sort(range(size: moves.size),
           test: method(x,y)
                     if (away) \> else \< end (move-distance[x], move-distance[y])
                 end);
  move-indices := choose(method(x)
                             move-distance[x]
                             = move-distance[move-indices[0]]
                         end,
                         move-indices);
  moves := map(curry(element, moves), move-indices);
  
  /*    for (move in moves)
          dbg("MOVE: %s %s %s %s\n",
              other-cop.player-name,
              move-distance[move-indices[0]],
              move.target.node-name,
              move.transport);
        end for;
    */
  moves;
end method;

define method generate-moves (player :: <player>,
                              #key keep-current-transport = #f)
  => (move :: <simple-object-vector>)
  let move = make(<move>,
                  target: player.player-location,
                  transport: player.player-type,
                  bot: player);
  generate-moves(move, keep-current-transport: keep-current-transport);

end method;

define method generate-moves(move :: <move>,
                             #key keep-current-transport = #f)
 => (moves :: <simple-object-vector>)
  let options = make(<stretchy-vector>, size: 16);
  options.size := 0; // preallocate space hack

  local method add-to-options (list :: <stretchy-object-vector>, transport :: <string>)
          for (tar :: <node> in list)
            add!(options, make(<move>,
                               target: tar,
                               transport: transport,
                               bot: move.bot));
          end;
        end method;

  if (move.transport = "robber")
    add-to-options(move.target.moves-by-foot, "robber");
  else
    //dbg("generate-moves transport = %s keep = %=\n", move.transport, keep-current-transport);
    if ((move.transport = "cop-foot") |
          (~keep-current-transport & (move.target.node-tag = "hq")))
      add-to-options(move.target.moves-by-foot, "cop-foot");
      //dbg("adding foot moves\n");
    end;
    if ((move.transport = "cop-car") | 
          (~keep-current-transport & (move.target.node-tag = "hq")))
      add-to-options(move.target.moves-by-car, "cop-car");
      //dbg("adding car moves\n");
    end;
  end if;

  //for (ele in options)
  //  dbg("GENMOVE: %= %=\n", ele.target.node-name, ele.transport);
  //end;

  as(<simple-object-vector>, options);
end method;

define method random-player-move (player :: <player>) => (move :: <move>)
  let possible = generate-moves(player);
  possible[random(possible.size)];
end method;

define method smelled-nodes(player :: <player>)
  let (res1, res2) = smelled-nodes-aux(player);
  if (player.player-type = "cop-car")
    res1;
  else
    concatenate(res1, res2);
  end if;
end method;

define method smelled-nodes-aux(player :: <player>)
  let move = make(<move>,
                  target: player.player-location,
                  transport: "cop-foot",
                  bot: player);
  let (first-rank, first-nodes) = distance(player, //not used
                                           move.target,
                                           source: move,
                                           maximum-rank: 1,
                                           keep-current-transport: #t);
  let (second-rank, second-nodes)
    = distance(player, //not used
               move.target,
               source: move,
               maximum-rank: 2,
               keep-current-transport: #t);

  values (first-nodes, second-nodes);
end method;

define method generate-plan(world :: <world>,
                            player :: <player>,
                            move :: <move>)
  => (plan :: <plan>)
  make(<plan>,
       bot: player.player-name,
       location: move.target,
       type: move.transport,
       world: world.world-number + 1);
end method;

define method generate-inform(world :: <world>,
                              node :: <node>,
                              certainty :: <integer>,
                              #key number :: <integer> = world.world-number)
 => (inform :: <inform>)
  make(<inform>,
       bot: world.world-skeleton.robber-name,
       location: node,
       type: "robber",
       world: number,
       certainty: certainty);
end method;

define method generate-informs (world, probability-map, list) => (informs)
  let res = #();
  for (node in list)
    res := add(res,
               generate-inform
                 (world,
                  node,
                  truncate
                    (if (probability-map[node.node-id] = 0.0s0)
                       -100;
                     else
                       probability-map[node.node-id] * 100
                     end if)));
    //dbg("MYINFORM %s %s %s\n", res.head.plan-location.node-name,
    //    res.head.inform-certainty, res.head.plan-world);
  end for;
  do(curry(add!, world.world-informs),
     map(rcurry(pair, world.world-my-player.player-name), res));
  res;
end method;

define method normalize! (map :: <vector>)
  let sum = reduce1(\+, map);
  for (elt in key-sequence(map))
    map[elt] := map[elt] / sum;
  end for;
  map;
end method;
    


limited-vector-class(<int-vector>, <integer>, 0);

lock-down <int-vector> end;

define function distance
    (player :: <player>,
     target-node :: <node>,
     #key source :: <move>
       = make(<move>,
              target: player.player-location,
              transport: player.player-type,
              bot: player),
     keep-current-transport = #f,
     maximum-rank = #f)
 => (rank :: <integer>, shortest-path :: <list>)

  let distance-to =
    make(<int-vector>, size: maximum-node-id(), fill: maximum-node-id());
  distance-to[source.target.node-id] := 0;
  let shortest-path :: <simple-object-vector> =
    make(<vector>, size: maximum-node-id(), fill: #());

  let todo-nodes = make(<deque>);

  local method search (start :: <move>)
         => (next-node-id :: <integer>);
          let start-id = start.target.node-id;
          let path-to-start = shortest-path[start-id];
          let next-distance = distance-to[start-id] + 1;
          block (return)
            if (maximum-rank & (maximum-rank < next-distance))
              return(23);
            end if;
            for (next :: <move> in
                   generate-moves(start,
                                  keep-current-transport: keep-current-transport))
              let next-id = next.target.node-id;
              if (distance-to[next-id] > next-distance)
                distance-to[next-id] := next-distance;
                shortest-path[next-id] := add!(path-to-start, next);
                push-last(todo-nodes, next);
              end if;
              if ((next-id == target-node.node-id)
                    & (maximum-rank = #f))
                return(next-id);
              end if;
            end for;
            if (todo-nodes.size = 0)
              error("Graph not connected");
            end if;
            search(todo-nodes.pop);
          end;
        end method;

  let destination-id = search(source);
  if (maximum-rank)
    //we want to get all nodes with distance = maximum-rank
    values(maximum-rank,
           map(find-node-by-id, choose(method(x)
                                           distance-to[x] = maximum-rank;
                                       end,
                                       key-sequence(distance-to))));
  /*dbg("LOC: %s TARGET: %s\n", player.player-location.node-name,
      target-node.node-name);
  for (i from 0 below maximum-node-id())
    if (size(shortest-path[i]) > 0)
      dbg("SP TO %d, distance: %d  ", i, distance-to[i]);
      for (j in shortest-path[i])
        dbg("%s ", j.target.node-name);
      end for;
      dbg("\n");
    end if;
  end for;*/
  else
    let res :: <list> = shortest-path[destination-id];
    values(distance-to[destination-id], res.reverse);
  end if;
end;

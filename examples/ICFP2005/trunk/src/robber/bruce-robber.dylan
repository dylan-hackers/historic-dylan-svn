module: bruce-robber


define class <bruce-robber> (<robber>)
  slot goal-banks :: <stretchy-object-vector> = make(<stretchy-vector>);
end class <bruce-robber>;

register-bot(<bruce-robber>);

define function find-accessable-banks(robber :: <bruce-robber>, world :: <world>)
 => ();
  if (robber.goal-banks.empty?)
    let nodes-by-id = world.world-skeleton.world-nodes-by-id;

    for (bank :: <bank> in world.world-banks)
      let node = bank.bank-location;
      let reachable = make(<vector>, size: nodes-by-id.size);
      let escape-routes = node.moves-by-foot;
      for (node :: <node> in escape-routes)
        reachable[node.node-id] := #t;
      end;
      let reachable-next = make(<vector>, size: nodes-by-id.size);
      for (i from 0 below reachable.size)
        if (reachable[i])
          for (node :: <node> in nodes-by-id[i].moves-by-foot)
            reachable-next[node.node-id] := #t;
          end;
          reachable-next[i] := #t;
        end;
      end;
      
      let numReachable = 0;
      for (ok in reachable-next)
        if (ok)
          numReachable := numReachable + 1;
        end
      end;

      dbg("bank at %d numreachable in two moves is %d\n", node.node-id, numReachable);
      if (numReachable >= 10)
        add!(robber.goal-banks, bank.bank-location);
      end;
    end;
  end;
end find-accessable-banks;


define constant *cop-probability* = 100000000;

define function count-greater-than(v :: <int-vector>, min :: <integer>)
 => (count :: <integer>, total-prob :: <integer>);
  let count = 0;
  let prob = 0;
  for (i in v)
    if (i > min)
      count := count + 1;
    end;
    prob := prob + i;
  end;
  values(count, prob);
end count-greater-than;


define method choose-move(robber :: <bruce-robber>, world :: <world>)
  find-accessable-banks(robber, world);
  let max-iterations = 6;
  let node-lookup = world.world-skeleton.world-nodes-by-id;
  let num-nodes = node-lookup.size;
  let num-cops = world.world-cops.size;

  // for each future world, a vector keyed by cop number, containing a vector of probabilities
  let cop-total-prob = make(<vector>, size: max-iterations);
  for (i from 0 below max-iterations)
    cop-total-prob[i] := make(<vector>, size: num-cops);
  end;

  let hq-node =
    block (return)
      for (node :: <node> in node-lookup)
        if (node.node-tag = "hq")
          return(node);
        end;
      end;
    end;

  for (cop :: <player> in world.world-cops, cop-number from 0)
    let cop-foot-prob = make(<int-vector>, size: num-nodes, fill: 0);
    let cop-car-prob = make(<int-vector>, size: num-nodes, fill: 0);

    // set up tables, knowing where cops are right now
    if (cop.player-type = "cop-foot")
      cop-foot-prob
    else
      cop-car-prob
    end [cop.player-location.node-id] := *cop-probability*;

    // save current positions first (we don't want to step on one!)
    let totals = make(<int-vector>, size: num-nodes, fill: 0);
    totals[cop.player-location.node-id] := *cop-probability*;
    cop-total-prob[0][cop-number] := totals;

    for (round-num from 1 below max-iterations)
      let new-cop-foot-prob = make(<int-vector>, size: num-nodes, fill: 0);
      let new-cop-car-prob = make(<int-vector>, size: num-nodes, fill: 0);

      local method spread-probs(probs :: <int-vector>,
                                new-probs :: <int-vector>,
                                move-selector) => ();
              for (i from 0 below num-nodes)
                let prob = probs[i];
                if (prob > 0)
                  let node = node-lookup[i];
                  let curr-node-id = node.node-id;
                  if (node == hq-node)
                    let prob = truncate/(prob, node.moves-by-foot.size + node.moves-by-car.size + 2);
                    let foot-moves :: <stretchy-object-vector> = node.moves-by-foot;
                    let car-moves :: <stretchy-object-vector> = node.moves-by-car;
                    for (move :: <node> in foot-moves)
                      let target-id = move.node-id;
                      new-cop-foot-prob[target-id] := new-cop-foot-prob[target-id] + prob;
                    end;
                    for (move :: <node> in car-moves)
                      let target-id = move.node-id;
                      new-cop-car-prob[target-id] := new-cop-car-prob[target-id] + prob;
                    end;
                    new-cop-foot-prob[curr-node-id] := new-cop-foot-prob[curr-node-id] + prob;
                    new-cop-car-prob[curr-node-id] := new-cop-car-prob[curr-node-id] + prob;
                  else
                    let moves :: <stretchy-object-vector> = node.move-selector;
                    let prob = truncate/(prob, moves.size + 1);
                    for (move :: <node> in moves)
                      let target-id = move.node-id;
                      new-probs[target-id] := new-probs[target-id] + prob;
                    end for;
                    new-probs[curr-node-id] := new-probs[curr-node-id] + prob;
                  end if; //hq
                end if; //prob nonzero
              end for; // all nodes
            end method spread-probs;

      spread-probs(cop-foot-prob, new-cop-foot-prob, moves-by-foot);
      spread-probs(cop-car-prob, new-cop-car-prob, moves-by-car);

      let totals = make(<int-vector>, size: num-nodes, fill: 0);
      for (i from 0 below num-nodes)
        totals[i] := new-cop-foot-prob[i] + new-cop-car-prob[i];
      end;
      cop-total-prob[round-num][cop-number] := totals;
      cop-foot-prob := new-cop-foot-prob;
      cop-car-prob := new-cop-car-prob;

      /*
      let (foot-places, foot-prob) = count-greater-than(cop-foot-prob, 0);
      let (car-places, car-prob) = count-greater-than(cop-car-prob, 0);
      dbg("round %d, places cop %s could be, foot: %d (%d), car: %d (%d)\n",
          round-num,
          cop.player-name,
          foot-places, foot-prob,
          car-places, car-prob);
      */

    end for; // iterating this cop probabilities
  
    /*
      dbg("\n\nby foot probability function: ");
      for (i in cop-foot-prob)
        dbg("%d  ", i);
      end;
    dbg("\n\nby car probability function: ");
    for (i in cop-car-prob)
      dbg("%d  ", i);
    end;
    dbg("\n\n");
    */

  end for; // each cop

  let my-location = world.world-robber.player-location;

  let (distances-to, safest-paths) =
    find-safe-paths(cop-total-prob,
                    my-location);
  
  let shortest-path-len = 2000000001;
  let shortest-path = #();
  let smallest-diff = 1000000;
  for (bank :: <bank> in world.world-banks)    
    if (bank.bank-money > 0 &
          member?(bank.bank-location, robber.goal-banks))
          //cop-total-prob[0][bank.bank-location.node-id] == 0)
      let bank-node-id = bank.bank-location.node-id;
      let path = safest-paths[bank-node-id];
      let dist = distances-to[bank-node-id];
      let diff = dist + dist - path.size;
      if (dist < shortest-path-len)
      //if (diff < smallest-diff)
        smallest-diff := diff;
        shortest-path-len := dist;
        shortest-path := path;
      end;
    end;
  end for;

  let shortest-path = shortest-path.reverse;
  let next-node = shortest-path.head;

  dbg("safest path (len %d, score %d) = %=\n",
      shortest-path.size, shortest-path-len,
      map(node-id, shortest-path));

  if (shortest-path.empty?)
    dbg("too hot for me .. let's get outta here...\n");
    
    // find the safest node we can reach
    let danger :: <int-vector> = cop-total-prob[0];
    let safest-place = my-location;
    let best-safety = danger[safest-place.node-id];
    for (node :: <node> in my-location.moves-by-foot)
      block (next)
        let safety = danger[node.node-id];
        dbg("safety at %d = %d\n", node.node-id, safety);
        if (safety <= best-safety)
          // make sure it's not a bank
          for (bank :: <bank> in world.world-banks)
            let bank-node = bank.bank-location;    
            if (node == bank-node)
              dbg("dont rob that bank!!\n");
              next();
            end;
          end;

          best-safety := safety;
          safest-place := node;
        end if;
      end block;
    end for;
    
    dbg("instead we'll go to %d with safety %d\n", safest-place.node-id, best-safety);
    next-node := safest-place;
  end;


  for (bank :: <bank> in world.world-banks)
    let bank-node = bank.bank-location;    
    if (next-node == bank-node)
      dbg("hey!!! we're about to rob a bank!!\n");
      //robber.goal-bank := next-bank(world, robber.goal-bank);
    end;
  end;

  make(<robber-move>, target: next-node, transport: "robber", bot: world.world-my-player);

end method choose-move;


        

define function find-safe-paths
    (danger :: <simple-object-vector>,
     from-node :: <node>)
 => (distance-to :: <int-vector>, shortest-paths :: <simple-object-vector>)

  let density-round-max = danger.size - 1;

  let current-positions :: <simple-object-vector> = danger[0];
  let immediate-danger :: <simple-object-vector> = danger[1];
  let smell-range :: <simple-object-vector> = danger[2];

  let cost-to =
    make(<int-vector>, size: maximum-node-id(), fill: 2000000000);
  let distance-to =
    make(<int-vector>, size: maximum-node-id(), fill: 2000000000);
  cost-to[from-node.node-id] := 0;
  distance-to[from-node.node-id] := 0;
  let shortest-path :: <simple-object-vector> =
    make(<vector>, size: maximum-node-id(), fill: #());

  let todo-nodes = make(<deque>);
  let fudge :: <integer> = truncate/(*cop-probability*, 100);

  local method search (start :: <node>) => ();
          let start-id = start.node-id;
          let path-to-start = shortest-path[start-id];
          let cost-to-start = cost-to[start-id];
          let distance-to-at-start = distance-to[start-id];
          let density-round-num = distance-to-at-start + 2;
          if (density-round-num > density-round-max)
            density-round-num := density-round-max;
          end;
          let cop-density :: <simple-object-vector> = danger[density-round-num];

          block (return)
            let moves :: <stretchy-object-vector> = start.moves-by-foot;
            for (next :: <node> in moves)
              let next-id = next.node-id;
              
              let current-position-level = 0;
              let imminent-danger-level = 0;
              let smell-level = 0;
              let cop-probability = 0;

              for (cop-num from 0 below current-positions.size)
                local method fetch(table :: <simple-object-vector>, cop-num :: <integer>, next-id :: <integer>)
                       => (danger :: <integer>);
                        let dangers :: <int-vector> = table[cop-num];
                        dangers[next-id];
                      end method fetch;
                current-position-level := current-position-level +
                  if (fetch(current-positions,cop-num,next-id) > 0) 1 else 0 end;
                imminent-danger-level := imminent-danger-level +
                  if (fetch(immediate-danger,cop-num,next-id) > 0) 1 else 0 end;
                smell-level := smell-level +
                  if (fetch(smell-range,cop-num,next-id) > 0) 1 else 0 end;
                cop-probability := cop-probability + fetch(cop-density,cop-num,next-id);
              end;
              let penalty =
                100000 * current-position-level +
                100 * imminent-danger-level +
                case
                  //smell-level > 0 => 5;
                  cop-probability == 0 => 0;
                  //cop-probability >= *cop-probability* => 999999;
                  otherwise => //dbg("prob penalty of %d\n",
                               //    round/(cop-probability, fudge * density-round-num));
                    round/(cop-probability, fudge /* * density-round-num */);
                end;

              let cost = cost-to-start + penalty + 1;
              if (cost-to[next-id] > cost)
                cost-to[next-id] := cost;
                shortest-path[next-id] := add!(path-to-start, next);
                push-last(todo-nodes, next);
              end if;
            end for;
            if (todo-nodes.size = 0)
              return();
              //error("Graph not connected");
            end if;
            search(todo-nodes.pop);
          end;
        end method;

  search(from-node);
  values(cost-to, shortest-path);
end;
